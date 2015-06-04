//
//	PROGRAM:
//		dcpq.exe
//
//	DESCRIPTION:
//		Run this on a domain controller
//		make sure that 
//			adfind.exe
//			portqry.exe
//			nslookup.exe
//		are available in the path
//
//		Get all trusts from AD 
//		Determine the Domain Controllers of the trusted domains
//		Check with portqry.exe the ports.
//
//	VERSION:
//		01	2015-06-03	Initial version
//
//
//		portqry -n 10.0.0.1 -e 53 -p UDP
//



program DomainControllerPortQuery;



uses
  Classes, 
  Process, 
  SysUtils,
  USupportLibrary;


  
 const
	TAB = #9;
  

type
	RQueryPorts = record
		localIp: string;
		remoteIp: string;
		port: string;
		protocol: string;
		status: string;
	end;
	
	TQueryPorts = array of RQueryPorts;
  
	RPort = record
		port: string;
		portDescription: string;
		protocol: string;
	end;
  
	TPort = array of RPort;


	
 var
	qp: TQueryPorts;
	arrayPort: TPort;
	localIp: string;



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
	WriteLn('DoPortQuery(' + remoteIp + ', ' + port + ', ' + protocol + ')');

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
    p.Parameters.Add('/c adfind.exe -b "CN=System,DC=prod,DC=ns,DC=nl" -f "(objectClass=trustedDomain)" trustPartner -csv  -nodn -nocsvheader -nocsvq >trusts.tmp');
	p.Options := [poWaitOnExit];	// Wait until the external program is finished.
	p.Execute;
end; // of procedure GetAllDomainTrusts



procedure PortQueryAdd(newLocalIp: string; newRemoteIp: string; newPort: string; newProtocol: string);
var
	i: integer;
begin
	i := Length(qp);
	
	SetLength(qp, i + 1);
	qp[i].localIp := newLocalIp;
	qp[i].remoteIp := newRemoteIp;
	qp[i].port := newPort;
	qp[i].protocol := newProtocol;
end; // of procedure PortQueryAdd.



procedure PortQueryShow();
var
	i:  integer;
begin
	WriteLn;
	WriteLn('PortQueryShow()');
	for i := 0 to High(qp) do
	begin
		WriteLn(qp[i].localIp + Chr(9) + qp[i].remoteIp + Chr(9) + qp[i].port + Chr(9) + qp[i].protocol);
	end;
end;



procedure PortQueryOnAll();
var
	i: integer;
	r: integer;
begin
	WriteLn;
	WriteLn('PortQueryOnAll()');
	for i := 0 to High(qp) do
	begin
		//WriteLn(qp[i].localIp + Chr(9) + qp[i].remoteIp + Chr(9) + qp[i].port + Chr(9) + qp[i].protocol);
		r := DoPortQuery(qp[i].remoteIp, qp[i].port, qp[i].protocol);
		WriteLn(TAB, 'RESULT=', r);
	end;
end;



procedure PortAdd(newPort: string; newProtocol: string);
var
	i: integer;
begin
	i := Length(arrayPort);
	
	SetLength(arrayPort, i + 1);
	
	arrayPort[i].port := newPort;
	arrayPort[i].protocol := newProtocol;
end;



procedure PortShow();
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
			
				//WriteLn('>>', newLine);
				if Length(remoteIp) > 0 then
				begin
					for i := 0 to High(arrayPort) do
					begin
						PortQueryAdd(localIp, remoteIp, arrayPort[i].port, arrayPort[i].protocol);
					end;
				end;
			end;
		end;
	until Eof(f);
	Close(f);
	
	// vm70as003.rec.nsint, WSUS, McAfee EPO
	PortQueryAdd(localIp, '10.4.222.17', '80', 'TCP');
	PortQueryAdd(localIp, '10.4.222.17', '443', 'TCP');
	PortQueryAdd(localIp, '10.4.222.17', '8081', 'TCP');
	PortQueryAdd(localIp, '10.4.222.17', '8443', 'TCP');
	PortQueryAdd(localIp, '10.4.222.17', '8444', 'TCP');
	PortQueryAdd(localIp, '10.4.222.17', '8530', 'TCP');
	
	// VM70AS004.rec.nsint, SCOM
	PortQueryAdd(localIp, '10.4.222.18', '5723', 'TCP');
	
	// VM70AS006.rec.nsint, Splunk SMB
	PortQueryAdd(localIp, '10.4.222.20', '445', 'TCP');
				
	// VM00AS1346.prod.ns.nl KMS
	PortQueryAdd(localIp, '10.4.139.104', '1688', 'TCP');
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


begin
	localIp := GetLocalIp();

	WriteLn('Local IP=', localIp);
	
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
	PortQueryShow();
	PortQueryOnAll();

	//WriteLn(DoPortQuery('10.4.68.21', '389', 'TCP'));
	
	//WriteLn(DoPortQuery('10.146.1.15', '464', 'TCP'));
	
end. // of program TestLaunchProcessAdFind.