//
//	PROGRAM:
//		PortQueryDomainController.exe (pqdc)
//
//	DESCRIPTION:
//		Run this on a domain controller
//		make sure that 
//			adfind.exe
//			portqry.exe
//			nslookup.exe
//		are available in the path
//
//		1) Get all trusts from AD using ADFIND.EXE > GetAllDomainTrusts
//		2) Determine the Domain Controllers of the trusted domains using a DNS nslookup > GetIpsPerDnsDomain
//		3) Check with portqry.exe the ports.
//
//	VERSION:
//		01	2015-06-03	Initial version
//		
//
//		portqry -n 10.0.0.1 -e 53 -p UDP
//
//
//	FUNCTIONS AND PROCEDURES:
//		function DoPortQuery
//		procedure GetAllDomainTrusts
//		procedure PortQueryAdd
//		procedure PortQueryShow
//		procedure PortQueryShowWithResult
//		procedure ExportResultToCsv
//		procedure ExportResultToSql
//		procedure PortQueryOnAll
//		procedure PortAdd
//		procedure PortShow
//		procedure GetIpsPerDnsDomain
//		procedure GetAllDcIpPerDnsDomain
//



program DomainControllerPortQuery;


{$MODE OBJFPC}


uses
  Classes, 
  Process, 
  SysUtils,
  USupportLibrary;


  
 const
	TAB = 					#9;
	VERSION =				'01';
	DESCRIPTION =			'PortQueryDomainController';
	ID = 					'142';		


type
	RQueryPorts = record
		checked: string;		// The date and time when checked
		localIp: string;		// The local IP of the current server.
		remoteIp: string;		// The remote IP to check the port on.
		port: string;			// The port number to check.
		protocol: string;		// The protocol to test: UDP or TCP.
		status: integer;		// Result of the portqry. 0=OK, 1=NOT LISTENING, 2=FILTERED or LISTENING
	end;

	TQueryPorts = array of RQueryPorts;

	
	RPort = record
		port: string;
		portDescription: string;
		protocol: string;
	end;

	TPort = array of RPort;


	
 var
	arrayQueryPorts: TQueryPorts;
	arrayPort: TPort;
	localIp: string;
	fileNameOut: string;
	giTotalPortsToCheck: integer;
	rootDse: string;
	gbDoResolve: boolean;				// Turn on or off the resolving of IP addresses.
	gbDoCsv: boolean;
	gbDoSql: boolean;
	gsLocalFqdn: string;				// Local FQDN.
	ip: string;



function DoPortQuery(remoteIp: string; port: string; protocol: string): integer;
//
// -n <IP or hostname>
// -e <port>
// -p <protocol (TCP or UDP)>
// -q 'quiet' operation runs with no output
//	
// returns 0 if port is listening
// returns 1 if port is not listening
// returns 2 if port is listening or filtered
//
var
	p: TProcess;
begin	
	//WriteLn('DoPortQuery(' + remoteIp + ', ' + port + ', ' + protocol + ')');

	p := TProcess.Create(nil);
	p.Executable := 'cmd.exe'; 
    p.Parameters.Add('/c portqry.exe -n ' + remoteIp + ' -e ' + port + ' -p ' + protocol + ' -q');
	p.Options := [poWaitOnExit];

	p.Execute;
	
	DoPortQuery := p.ExitStatus; 
end; // of procedure DoPortQuery.



procedure GetAllDomainTrusts();
//
//	Use ADFIND to make a list to get all trusts into a file trust.tmp
//
var
	p: TProcess;
begin
	WriteLn;
	WriteLn('GetAllDomainTrusts()');

	p := TProcess.Create(nil);

	p.Executable := 'cmd.exe'; 
    p.Parameters.Add('/c adfind.exe -b "CN=System,' + rootDse + '" -f "(objectClass=trustedDomain)" trustPartner -csv  -nodn -nocsvheader -nocsvq >trusts.tmp');
	p.Options := [poWaitOnExit];	// Wait until the external program is finished.
	p.Execute;
end; // of procedure GetAllDomainTrusts



function ResolveFqdnDc(ip: string): string;
//
// Resolve the IP to an FQDN
// Redo it until the result doesn't contain:
//	1)	._msdcs in the FQDN
//	2) 	the FQDN <> current domain name.
//
var
	bCorrect: boolean;
	x: integer;
	fqdn: string;
	sDnsDomain: string;
begin
	
	bCorrect := false;
	x := 0;
	sDnsDomain := GetDnsDomain();
	repeat
		x := x + 1;
		fqdn := ResolveFqdn(ip);
		//WriteLn(TAB, x, TAB, 'fqdn=', fqdn, TAB, sDnsDomain);
		
		if (Pos('._msdcs.', fqdn) = 0) and (fqdn <> sDnsDomain) then
			bCorrect := true;
	until bCorrect = true;
	ResolveFqdnDc := fqdn;
end; // of function ResolveFdqnDc.



procedure PortQueryAdd(newLocalIp: string; newRemoteIp: string; newPort: string; newProtocol: string);
var
	i: integer;
begin
	i := Length(arrayQueryPorts);
	
	SetLength(arrayQueryPorts, i + 1);
	arrayQueryPorts[i].localIp := newLocalIp;
	arrayQueryPorts[i].remoteIp := newRemoteIp;
	arrayQueryPorts[i].port := newPort;
	arrayQueryPorts[i].protocol := newProtocol;
	
	giTotalPortsToCheck := giTotalPortsToCheck + 1
end; // of procedure PortQueryAdd.



procedure PortQueryShow();
var
	i:  integer;
begin
	WriteLn;
	WriteLn('PortQueryShow()');
	for i := 0 to High(arrayQueryPorts) do
	begin
		WriteLn(arrayQueryPorts[i].localIp + Chr(9) + arrayQueryPorts[i].remoteIp + Chr(9) + arrayQueryPorts[i].port + Chr(9) + arrayQueryPorts[i].protocol);
	end;
end;


procedure PortQueryShowWithResult();
var
	i:  integer;
begin
	WriteLn;
	WriteLn('PortQueryShowWithResult()');
	for i := 0 to High(arrayQueryPorts) do
	begin
		WriteLn(arrayQueryPorts[i].localIp + TAB + arrayQueryPorts[i].remoteIp + TAB + arrayQueryPorts[i].port + TAB + arrayQueryPorts[i].protocol + TAB + IntToStr(arrayQueryPorts[i].status));
	end;
end;



procedure ExportResultToCsv();
//
// Export the data in the arrayQueryPorts to an Excel CSV file.
// Separator char is ; (#59)
//
const
	SEP = #59; // ;

var
	i: integer;
	f: TextFile;
	buffer: string;
begin
	AssignFile(f, fileNameOut + '.csv');

	ReWrite(f);
	
	WriteLn;
	WriteLn('ExportResultToCsv() ' + fileNameOut + '.csv');
	for i := 0 to High(arrayQueryPorts) do
	begin
		// Add the local IP to the buffer.
		buffer := arrayQueryPorts[i].localIp + SEP;
		
		// Add the local FQDN to the buffer.
		if gbDoResolve = true then
			buffer := buffer + gsLocalFqdn + SEP;
		
		// Add the remote Ip to the buffer.
		buffer := buffer + arrayQueryPorts[i].remoteIp + SEP;
		
		// Add the FQDN of the remote IP to the buffer.
		if gbDoResolve = true then
			buffer := buffer + ResolveFqdnDc(arrayQueryPorts[i].remoteIp) + SEP;
			
		// Add the port to buffer.
		buffer := buffer + arrayQueryPorts[i].port + SEP;
			
		// Add the protocol to the buffer.
		buffer := buffer + arrayQueryPorts[i].protocol + SEP;
	
		// Add the status of the PortQry to the buffer.
		// Issue#3: fix. Do not export the number but letters for port query status:
		//  L = Listening.
		//	F = Failed listening port.
		if arrayQueryPorts[i].status = 0 then
			buffer := buffer + 'L'
		else
			buffer := buffer + 'F';
	
		// Write to screen.
		//WriteLn(buffer);
		
		// Write to the CSV file.
		WriteLn(f,buffer);
	end;
	CloseFile(f);
end;



procedure ExportResultToSql();
const
	SEP = #59;

var
	i: integer;
	f: TextFile;
	sql: string;
begin
	AssignFile(f, fileNameOut + '.sql');

	ReWrite(f);
	
	WriteLn;
	WriteLn('ExportResultToSql() ' + fileNameOut + '.sql');
	for i := 0 to High(arrayQueryPorts) do
	begin
		//WriteLn(arrayQueryPorts[i].localIp + TAB + arrayQueryPorts[i].remoteIp + TAB + arrayQueryPorts[i].port + TAB + arrayQueryPorts[i].protocol + TAB + IntToStr(arrayQueryPorts[i].status));
		//WriteLn(f,arrayQueryPorts[i].localIp + SEP + arrayQueryPorts[i].remoteIp + SEP + arrayQueryPorts[i].port + SEP + arrayQueryPorts[i].protocol + SEP + IntToStr(arrayQueryPorts[i].status));
		sql := 'INSERT INTO system_port_query ';
		sql := sql + 'SET ';
		//WriteLn(DateTimeToStr(Now));
		//sql := sql + 'check_datetime=''' + GetProperDateTime(Now()) + ''',';
		sql := sql + 'check_datetime=''' +arrayQueryPorts[i].checked + ''',';
		sql := sql + 'local_ip=''' + arrayQueryPorts[i].localIp + ''',';
		sql := sql + 'remote_ip=''' + arrayQueryPorts[i].remoteIp + ''',';
		sql := sql + 'port=' + arrayQueryPorts[i].port + ',';
		sql := sql + 'protocol=''' + arrayQueryPorts[i].protocol + ''',';
		sql := sql + 'status=' + IntToStr(arrayQueryPorts[i].status) + ';';
		
		WriteLn(f, sql);
	end;
	CloseFile(f);
end;


procedure PortQueryOnAll();
var
	i: integer;
	r: integer;
begin
	WriteLn;
	WriteLn('PortQueryOnAll()');
	for i := 0 to High(arrayQueryPorts) do
	begin
		//WriteLn(arrayQueryPorts[i].localIp + Chr(9) + arrayQueryPorts[i].remoteIp + Chr(9) + arrayQueryPorts[i].port + Chr(9) + arrayQueryPorts[i].protocol);
		r := DoPortQuery(arrayQueryPorts[i].remoteIp, arrayQueryPorts[i].port, arrayQueryPorts[i].protocol);
		
		arrayQueryPorts[i].status := r;
		
		// Issue-5: Write the current date time in the checked field.
		arrayQueryPorts[i].checked := GetProperDateTime(Now());
		
		// Fix issue#2: array loop is from 0 to high, display is i + 1.
		WriteLn(TAB, i + 1, '/', giTotalPortsToCheck, ':', TAB, arrayQueryPorts[i].remoteIp, ' (', arrayQueryPorts[i].port, '/', arrayQueryPorts[i].protocol, ')', TAB, 'RESULT=', r);
	end;
end;



procedure PortAdd(newPort: string; newProtocol: string);
//
// Add a new port to the query array.
//
var
	i: integer;
begin
	i := Length(arrayPort);
	
	SetLength(arrayPort, i + 1);
	
	arrayPort[i].port := newPort;
	arrayPort[i].protocol := newProtocol;
end;



procedure PortShow();
//
// Show all ports to query.
//
var
	i:  integer;
begin
	WriteLn;
	WriteLn('PortShow()');
	for i := 0 to High(arrayPort) do
	begin
		WriteLn(arrayPort[i].port + Chr(9) + arrayPort[i].protocol);
	end;
end;



procedure GetIpsPerDnsDomain(dns: string);
//
// Do a nslookup and resolve all IP addresses of Domaion Controllers an AD domain.
// Update the array to query the ports with PortQueryAdd().
//
var
	p: TProcess;
	f: TextFile;
	line: String;
	foundName: boolean;
	foundCount: integer;
	i: integer;
	remoteIp: string;
begin
	WriteLn;
	WriteLn('GetIpsPerDnsDomain(' + dns + ')');

	Sleep(1000);
	
	// Create a text file ipdc-domain.tmp
	p := TProcess.Create(nil);
	p.Executable := 'cmd.exe'; 
    p.Parameters.Add('/c nslookup -timeout=5 ' + dns + '>ipdc-' + dns + '.tmp');
	p.Options := [poWaitOnExit];
	p.Execute;
	
	foundName := false;
	
	// Open the text file and read the lines from it.
	Assign(f, 'ipdc-' + dns + '.tmp');
	
	{I+}
	Reset(f);
	repeat
		ReadLn(f, line);
		//WriteLn('GetIpsPerDnsDomain(): ',  line);
		
		If Pos('Name:', line) > 0 then
		begin
			// When the text 'Name:' is found. All the next lines contain 
			// IP addresses of the Domain Controllers of the domain.
			foundName := true;
			foundCount := 0;
		end;
		
		if foundName = true then
		begin
			foundCount := foundCount + 1;
			if foundCount > 1 then
			begin
				//WriteLn(Chr(9), 'ONLY THIS LINES: ', foundCount, Chr(9), line);
			
				// Remove all text 'Address:' from the line.
				remoteIp := Trim(StringReplace(line, 'Address:', '', [rfIgnoreCase]));
			
				// Remove all text 'Addresses:' from the newLine.
				remoteIp := Trim(StringReplace(remoteIp, 'Addresses:', '', [rfIgnoreCase]));
			
				// Only add a new port to query when:
				// 1) the remoteIP contains data,
				// 2) It's a IPv4 address, skip IPv6 addresses.
				if (Length(remoteIp) > 0) and (Pos(':', remoteIp) = 0) then
				begin
					for i := 0 to High(arrayPort) do
					begin
						// Increase the counter of tests to do.
						//giTotalPortsToCheck := giTotalPortsToCheck + 1;
						PortQueryAdd(localIp, remoteIp, arrayPort[i].port, arrayPort[i].protocol);
					end;
				end;
			end;
		end;
	until Eof(f);
	Close(f);
end; // of procedure GetIpsPerDnsDomain.



procedure GetAllDcIpPerDnsDomain();
//
//	Open the file trusts.tmp and obtain all IP addresses of domain controllers from a a domain.
//
var
	f: TextFile;
	line: String;
begin
	WriteLn;
	WriteLn('GetAllDcsPerDnsDomain()');
	Assign(f, 'trusts.tmp');
	
	{I+}
	
	Reset(f);
	repeat
		ReadLn(f, line);
		//WriteLn(s);
		GetIpsPerDnsDomain(LowerCase(line));
	until Eof(f);
	Close(f);
end; // of procedure GetAllDcsPerDnsDomain.




procedure ProgramTitle();
begin
	WriteLn();
	WriteLn(StringOfChar('-', 80));
	WriteLn(UpperCase(GetProgramName()) + ' -- Version: ' + VERSION + ' -- Unique ID: ' + ID);
	WriteLn(DESCRIPTION);
	WriteLn(StringOfChar('-', 80));	
end; // of procedure ProgramTitle()



procedure ProgramUsage();
var
	p:	string;
begin
	p := ParamStr(0);

	WriteLn();
	WriteLn('Switches:');
	WriteLn(TAB + '--csv                    Output in CSV format, seperator char is ;');
	WriteLn(TAB + '--sql                    Output in SQL format');
	WriteLn(TAB + '--resolve                Also resolve the IP address to FQDN''s');
	WriteLn(TAB + '--help, -h, -?           Show the help');
	WriteLn();
	WriteLn('Usage:');
	WriteLn(TAB + p + ' [switche(s)]');
	WriteLn();
end; // of procedure ProgramUsage()



procedure ProgInit();
var	
	i: integer;
begin
	// Process the parameters
	
	//WriteLn('ProgramInit()');
	//WriteLn('ParamCount: ', ParamCount);
	
	gbDoCsv := false;
	gbDoSql := false;
	gbDoResolve := false;
	
	ProgramTitle();
	
	if ParamCount = 0 then
	begin
		ProgramUsage();
		Halt(0);
	end
	else
	begin
		for i := 1 to ParamCount do
		begin
			//Writeln(i, ': ', ParamStr(i));
			
			case LowerCase(ParamStr(i)) of
				'--csv':
					begin
						gbDoCsv := true;
						WriteLn('Output in CSV format');
					end;
				'--sql':
					begin
						gbDoSql := true;
						WriteLn('Output in SQL format');
					end;
				'--resolve':
					begin
						gbDoResolve := true;
						WriteLn('FQDN resolve on');
					end;
				'--help', '-h', '-?':
					begin
						ProgramUsage();
						Halt(0);
					end;
			end; // of Case
		end; // of For
	end;
	
	localIp := GetLocalIp();
	
	if gbDoResolve = true then
		gsLocalFqdn := ResolveFqdnDc(localIp);
		
	rootDse := GetBaseDn();
	
	fileNameOut := 'pqdc-' + GetCurrentComputerName() + '-' + GetDateFs() + '_' + GetTimeFs();
	
	giTotalPortsToCheck := 0;
	
	gsLocalFqdn := localIp;
	
	WriteLn('Output in: ' + fileNameOut);
	WriteLn('Local IP:  ' + localIp);
	WriteLn('Root DSE:  ' + rootDse);
end; // of procedure ProgInit



procedure ProgRun();
begin
	PortAdd('88', 'TCP');
	PortAdd('135', 'TCP');
	PortAdd('389', 'TCP');
	PortAdd('445', 'TCP');
	PortAdd('464', 'TCP');
	PortAdd('3268', 'TCP');
	PortAdd('3269', 'TCP');
	
	//PortShow();

	GetAllDomainTrusts();
	GetAllDcIpPerDnsDomain();
	
	// Now add the extra servers and ports to the systems to query.
	
	// VM70AS003.rec.nsint, WSUS, McAfee EPO
	PortQueryAdd(localIp, '10.4.222.15', '3389', 'TCP'); 	// 1
	PortQueryAdd(localIp, '10.4.222.16', '3389', 'TCP'); 	// 2
	PortQueryAdd(localIp, '10.4.222.17', '80', 'TCP'); 		// 3
	PortQueryAdd(localIp, '10.4.222.17', '443', 'TCP'); 	// 4
	PortQueryAdd(localIp, '10.4.222.17', '8081', 'TCP'); 	// 5
	PortQueryAdd(localIp, '10.4.222.17', '8443', 'TCP');	// 6
	PortQueryAdd(localIp, '10.4.222.17', '8444', 'TCP');	// 7
	PortQueryAdd(localIp, '10.4.222.17', '8530', 'TCP');	// 8
	
	// VM70AS004.rec.nsint, SCOM
	PortQueryAdd(localIp, '10.4.222.18', '5723', 'TCP');	// 9
	
	// VM70AS006.rec.nsint, Splunk SMB
	PortQueryAdd(localIp, '10.4.222.20', '445', 'TCP');		// 10
				
	// VM00AS1346.prod.ns.nl KMS
	PortQueryAdd(localIp, '10.4.139.104', '1688', 'TCP');	// 11
	
	// Add DNS servers
	PortQueryAdd(localIp, '10.4.34.12', '53', 'TCP');		// 12
	PortQueryAdd(localIp, '10.4.34.11', '53', 'TCP');		// 13
	PortQueryAdd(localIp, '10.12.145.11', '53', 'TCP');		// 14
	
	// Count in the procedure PortQueryAdd
	//giTotalPortsToCheck := giTotalPortsToCheck + 14;
	
	WriteLn('There are ', giTotalPortsToCheck, ' ports found to be tested.');
	
	//PortQueryShow();
	
	PortQueryOnAll();
	
	//PortQueryShowWithResult();
	
	if gbDoCsv = true then
		ExportResultToCsv();
		
	if gbDoSql = true then
		ExportResultToSql();

	//WriteLn(DoPortQuery('10.4.68.21', '389', 'TCP'));
	
	//WriteLn(DoPortQuery('10.146.1.15', '464', 'TCP'));
end; // of procedure ProgInit



procedure ProgTest();
begin
	//WriteLn(GetBaseDn());
	//WriteLn(ResolveFqdn('10.145.193.15'));
	
	ip := '10.4.68.17';
	WriteLn('RESOLVE ' + ip + ' TO FQDN: ' + ResolveFqdnDc(ip));

	ip := '10.4.68.20';
	WriteLn('RESOLVE ' + ip + ' TO FQDN: ' + ResolveFqdnDc(ip));

	ip := '10.4.68.14';
	WriteLn('RESOLVE ' + ip + ' TO FQDN: ' + ResolveFqdnDc(ip));

	ip := '10.4.68.16';
	WriteLn('RESOLVE ' + ip + ' TO FQDN: ' + ResolveFqdnDc(ip));
end; // of procedure ProgTest






procedure ProgDone();
begin
end; // of procedure ProgInit



begin
	ProgInit();
	ProgRun();
	//ProgTest();
	ProgDone();
end. // of program PortQueryDomainController.