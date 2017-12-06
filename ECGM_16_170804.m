%function [AX, FF, ucl ]=ECGM_14_170801%(filemit);%102,104,107,217; 
if 18, if 2, if 12, if 10, if 5, if 4, if 3, if 1, 221
if  ~exist('Adrive' , 'var' ), clc ; close all ; clear all; pack ; end;  BF=10;  
if ~exist( 'Afilemit','var' ), Afilemit =233; end;  BL=10; Bthesis=10; track=1; 
zbestc = 2^4; TTFILE = [ 100; 103 ; 105 ; 111; 113; 117; 121; 123 ; 200 ; ...
 202; 210 ; 212 ; 213;  214 ; 219 ; 221 ; 222 ; 228 ; 231 ; 232; 233; 234;] ;
TRFILE = [ 101 ; 106 ; 108 ;109 ; 112 ;  114 ; 115 ; 116 ;118 ; 119; 122;  ...
 124; 201 ; 203 ; 205 ; 207 ;  208 ;  209 ;  215 ;  220;  223 ; 230; ]; 
 DN=3*360; comp= computer ;  bon=0;  ff=0;  Bst=0; Bsub=Bst;fonts=14;
 if ~exist('BVER', 'var'), BVER=10; end; B2696= 3; BAL= 'B'; BAL= 'U'; 
 if BL, B= 'BL'; else B = 'BLN'; end;qrs={'FontSize',14};LAGUNA=1;
 if (isequal(computer,'PCWIN64'))&&exist('G:\jaga','dir'),
Pmit='c:\data\mitdb\feature2017\';  %Afile1test = Afilemit ;   
 elseif  (isequal( computer,'PCWIN')), p =mfilename('fullpath')
addpath(genpath('D:\data\mitdb\featuref4\ecgpuwave')); 
 if Afilemit >400, Pmit = 'c:\data\mitdb\vfdb\' ; PATF = Pmit;  end;
addpath(genpath('D:\data\mitdb\featurenew\ecgpuwave')); 
Pmit='c:\data\mitdb\feature2017\';   %220 std xls
end; if ~isempty(intersect(Afilemit,TRFILE)), BO= 'TR'; 
elseif  ~isempty(intersect((Afilemit),TTFILE)), BO= 'TT';else BO='MANY';
end;  if 1,  PATFIG ='C:\FINAL\Dropbox\findr\' ; 
 %PATFIG ='C:\FINAL\Dropbox\MAIN\FIGU\' ; 
PATF ='c:\data\mitdb\mitdbset'; [sta,es,msid] =mkdir(PATF); 
cb='b' ; cy= 'y' ; cm = 'm' ;zzlen=[];scr = get(0, 'ScreenSize'); 
 fonts16=16;fonts14=14; cg= 'g' ;  cyan='c' ;  ck = 'k' ; cr= 'r' ;  cb= 'b' ; 
DISP=3*360;  znu0= 0; znum0= 0; znm0= 0; zn0=[]; z0=[];fonts18=18;
 if  ~exist('BI','var'),BI = 'INTER'; end;  Afilemits = num2str(Afilemit); 
SCR = get(0, 'ScreenSize');  FILEMITD = fullfile(Pmit , Afilemits) 
Header = readheader(  [ FILEMITD '.hea'] );  cd 'C:\FINAL\dropbox' ;
start = 1 ; NS = Header.nsamp ;  Bnan=0; % NS = 1080 ;  
fs = Header.freq ; TIM=(start:(start-1+3*360)) ;  
lead1= Header.desc(1,:); lead2= Header.desc(2,:); ACFILE1= mfilename
DN =start+DISP-1; PATXL= 'C:\FINAL\dropbox\'; file1= mfilename('fullpath')
L=NS-start+1; format  short g;   S=[]; screen = get(0,'ScreenSize'); 
stri1=[' ECG  ' Afilemits  ' Red  Channel 1 ' lead1 ', Blue Channel 2 ' lead2];
BR= 'DIR' ; R1000=[ ];   BCM  = 'O' ;  REE=[];  BANN= 10;  tic ; 
 if 1 ||  (~isequal( computer,'PCWIN')) ,
Annot=readannot([ FILEMITD , '.atr'],Header,[ start NS]);RO=Annot.time;
 elseif  isequal(pwd,'D:\code\MAT\ECG_FINAL'),   
Annot =readannot([ FILEMITD , '.atr'] ,[ start NS] ); RO =  Annot.time;
 end; if 1||  (isequal(pwd,'D:\jaga') || isequal(pwd,'d:\jaga')),  
% Annot= readannotp([FILEMITD, '.atr'] ,Header, [1 NS]); 
    Annot=readannot([ FILEMITD , '.atr' ],Header,  [ start NS] ); % m  file
Annottime =  Annot.time;   [  uRO  uROI  ] =  unique(  Annottime) ;
RO= Annottime(uROI ) ;  CO32=  Annot.anntyp(uROI ) ;
A38= setdiff([1: length(Annottime)] , uROI) ; cd 'C:\FINAL\dropbox' ;
if   exist( 'rdsign212' , 'file' ),
ecg2 = rdsign212([FILEMITD '.dat'], 2, 1,NS); %2 channels% if nosig==2,
min2= min(ecg2( start:DN ,2)) ; max1= max(ecg2( start:DN ,1)) ;
max2= max(ecg2( start:DN ,2)) ;    min1= min(ecg2( start:DN ,1)) ;
 amp1= min(min1,min2) ; amp2= max(max1,max2) ;clf; box on, hold on;
DBT= RO >= start & RO <= start+3*360-1 ;  DRT=RO(DBT);  grid off;
 h_1=figure('NAME',[' 50 ECG1 ' Afilemits ' Channel 1: Blue, Channel 2: Red'],...
 'Position', [10  40  screen(3)/2 screen(4)/3] ); % nan fslib113 228 
 plot(start:DN, ecg2(start:DN,1),  'r' , 'LineWidth', 2 ); hold on;  
 plot(start:DN, ecg2(start:DN,2),'b' , 'LineWidth', 2 );
 axis([start, DN, amp1-5.5, amp2+5.55 ]); title([ stri1] ,'color', 'r','FontSize',16);  
 for k=1:length(DRT)   text(RO(k), ecg2(RO(k),1), num2str( RO(k)) ); end;  
 end; TRAIN=100* [ 45868 942  3787 415    8 51020 ] / 51020  ;  
 TEST=100* [44259 1837 3221  388  7 49712  ]  / 49712  ; % dbclear all ; 
 TOTAL = 100* [90127 2779  7008 803    15  100732 ] / 100732 ;    
 TOTL =100* [51020 49712 ] / 100732 ;   DBUG=10;  
 end;end; if  1,dbstop 4106; dbstop 4181;DBUG=10;  
 dbclear at 1960 ; 
elseif  ~exist('Adrive','var'), dbstop   1395;  dbstop 2920; dbstop 3270;
dbstop 3440; dbstop 1850;dbstop 1905 ; dbstop 4106; dbstop 2805;;     
dbstop 3275; dbstop 2480;  dbstop 3290; dbstop 2880 ;dbstop 980 ;
dbstop 3830; dbstop 3940; dbstop 3380;  DBUG=10;dbstop 1770 ;
elseif  exist('Adrive','var'),  dbstop 1960;  dbstop 3440; DBUG=0;  
 dbstop 2240;  dbstop 3886; dbstop 2326;dbstop 1825; dbclear all ;
end; end;  if 10, [ xb , Rtime, ANN, xd ,  xu ] = ...
readsignal212m3( [ FILEMITD ] , 2, start,NS , PATFIG ); %2 channels
[uRO uROI ]=unique(Rtime); ROM=Rtime(uROI); iqrs= { 'FontSize', 12 } ;
 COM=ANN.anntyp ; ANNtime=single(ANN.time ) ;  
A69= setdiff([1: length(Rtime)] , uROI) ; iqrs= { 'FontSize', 14 }
A67= setdiff( ANN.time , Rtime) ;  RO = ( Rtime) ;  which norm -all ;  
ecg1 = xu(:,1);  xd1 = xd(:,1);     xd2 = xd(:,2) ;   x_a= xb;  
lead1= Header.desc(1,:); lead2= Header.desc(2,:);  clear xd;
ECG = genvarname([ 'ECG', Afilemits]); eval([ ECG '= xd1;']);
FILEQ=[ 'ECG' Afilemits '.mat' ];  FILEQ = fullfile(PATF, FILEQ ) 
if 0 ,  save( FILEQ ,ECG);  eval( ['clear ', ECG] ) ;   clear ECG;  end;
if 0 , sw =1800; p = 5 ;  AQRS =dpi_qrs(xd1 ,fs , sw,  p) ; end;
Aaux=Annot.aux;   eval(['clear ', ECG] ) ; clear ECG;  clear Tags h; 
  if Afilemit  >   400,   RO=  AQRS  ;     intersect(RV,RO)
for i =1: length( RO)    C(i) = ' ' ;    end;     CO=C ;    A.C=CO; 
else CO=ANN.anntyp;       C=Annot.anntyp; Asubtyp=Annot.subtyp ;
  if Afilemit == 220,  Aaux(1,:)=[];      CO(1)= [];  end; 
  end;  uANNCO=unique( C ) ; if 0&&DBUG, dbstop 96; end; 
[clq clqv]=find( C == '"'); ANNuO=unique(CO')
[annconp]=find( C ~='+'); ANNCONP = C(annconp); iqrs= { 'FontSize', 12 } ;
annconpt= find(ANNCONP~= '~' ) ; ANNCONPT = ANNCONP(annconpt) ;
annconpto =find(ANNCONPT~= '|' ); Annconpto= ANNCONPT(annconpto) ;
RCNQ=RO(clqv) ;   ROCNP=RO(annconp) ;  Cflutterl=[]; flutterl=0 ;
ROCNPT= ROCNP(annconpt) ;   ROCNPTO= ROCNPT(annconpto) ; 
clear   stri1  ANN    ANNTOTALCO ;   clear ecg2 ; CExclamR=[];
clear h  Rtime;  clear annconp annconpt annconpto   ROCNPTO ;
cnodal172 =0;  Cno=[]; Cnq=[];Rpacs=[];apcs=0; cpvcE =0; RpvcE=[] ;
cnodal =0;  Cnod=[]; Cje11=[] ; je11=0 ; Ccae34=[];  cae34= 0; 
CExclam=[]; exclam =0; CflutterR=[]; Cflutterl=[]; flutter=0 ;Cfsubtyp=[];
e=0; CElse=[]; CElseR=[];  m3=0;cnoisTilde=0;Cnon=[];CnoisTilde=[]; 
cbn=0;rm=0; cnpx=0;artif=0;fJ=0;fj=0;cvt=0;Cbbb=[];Cn=[];CNPX=[];Cf=[]; 
Cnorm=[];Rpac=[];Rpvc=[];Cbbr=[];Cbbl=[];CnoisTildeR=[]; CfJ=[];  rm2=0;
norm=0; apc=0; ab=0;  cpvc=0; l=0; r=0;   CfAux=[]; CnoisTildeSubtyp=[];
fusion=0;Rfus=[];rhycp=0; CPLUS=[];rhyc=0;  CNPXSubtyp=[] ; Cfj=[];
cpaceslash=0; CPaceSlashR= [];  CPaceSlash=[]; CnoisTildeAux=[];
cpacef=0; CPacefR= [];  CPacef=[]; CQR=[]; cQ=0;CQ=[]; CNPXAux=[];
CLASS={'AAMI1 Normal 1','AAMI1 LBBB 2','AAMI1 RBBB 3','AAMI2 Aap 4', ...
  'AAMI3 Pvc 5',  'AAMI4 Fusion 6' ,  'AAMI2 Jpre 7' , 'AAMI2 Apc 8',  ...     
 'AAMI2 Svpre 9','AAMI3 VentEsc 10','AAMI1 Je11','AAMI5  PSlash 12', ...
'AAMI5 unknown13','cnoisTilde14' ,'QRSW15','fORART16','RRR17','STCH18', ...  
  'TCH19', 'SYST20', 'DIAST21' , 'noteq"22' ,'meas23', 'pwav24', ...%24
  'bbb25', 'pacesp26', 'twavefor27', '+rhycplus28', 'uw29', 'learn30', 'flwav31',...
'vfon32', 'vfoff33' , 'AAMI1 e34', 'nsvesc35', 'link36', 'xbapc37', '5QfpN38',
};   CLAS= { 'AAMI1Normal1' , 'AAMI1LBBB2' , 'AAMI1RBBB3','AAMI2aap4', ...
  'AAMI3Pvc5',  'AAMI4Fusion6' ,  'AAMI2Jpre7' , 'AAMI2Apc8', ...     
 'Aami2Svpre9', 'AAMI3VentEsc10',  'AAMI1je11', '5QPSlash12', '5Quncl13', ...
 'cnoisTilde14' , 'QRSW15' , 'fORART16' ,   'RRR17','STCH18' , ... %16
  'TCH19', 'SYST20', 'DIAST21' , 'noteq"22' ,'meas23', 'pwav24', ...%24
  'bbb25', 'pacesp26', 'twavefor27', '+rhycplus28', 'uw29', 'learn30', 'flwav31',...
'vfon32', 'vfoff33' , 'AAMI1e34', 'nsvesc35', 'link36', 'xbapc37', '5QfpN38',
};   
 end;  for i =1: length(C)   %Annot.subtyp
   if (C(i) == 'N'),  norm= norm+1;     Cnorm(norm)=RO(i);  F(i) = 1; 
        elseif (C(i)== 'L') ,  l=l+1; Cbbl(l)=  i ;    F(i) =2;   %2LBBB
        elseif (C(i)== 'R') ,   r=r+1; Cbbr(r)=RO(i);       F(i) =3;     %3RBBB  
        elseif (C(i)== 'j') ,  je11= je11+1; Cje11(je11)=RO(i); F(i) = 11;  %11 
elseif (C(i)=='e') cae34= cae34+1; Ccae34(cae34)=RO(i);F(i) = 34; %34   
                
        elseif (C(i)== 'V') cpvc=cpvc+1;  Rpvc(cpvc)=RO(i);  F(i)=5; %VPC 5	
 elseif ( C(i)== 'E' ), cpvcE=cpvcE +1; RpvcE(cpvc)=RO(i);  F(i) = 10;%VPC     
 elseif (C(i)== 'F')  fusion=fusion +1; Rfus(fusion)= RO( i) ; F(i) =6;  %FUS6
      elseif (C(i)== 'a') ab=ab+1; Cab(ab)=RO(i);            F(i) = 4 ;   % 4 APC
elseif (C(i)== 'J'),   apc=apc+1; Rpac(apc)= RO(i); F(i) =  7;   %7 APC 234
        elseif (C(i)== 'A') apc=apc+1; Rpac(apc)=RO(i);  F(i) =  8;   %8;APC	 
elseif (C(i)== 'S' ) ,  apcs=apcs+1; Rpacs(apcs)=RO(i); F(i) = 9;% 9APC
elseif (C(i)== '/')  , cpaceslash=cpaceslash+1; CPaceSlash(cpaceslash)=i;
    CPaceSlashR(cpaceslash)=RO(i);   F(i)=12;  
elseif (C(i)== 'f') , cpacef=cpacef+1; CPacef(cpacef)=i;   
  CPacefR(cpacef)=RO(i);           F(i)= 38; 
elseif (C(i)== 'Q')  , cQ=cQ+1; CQ(cQ)=i;  CQR(cQ)=RO(i); F(i)= 13;    
 elseif (C(i)== '~') cnoisTilde = cnoisTilde+1;        
CnoisTildeSubtyp( cnoisTilde ) =  Asubtyp(i, :) ; 
CnoisTildeR(cnoisTilde)=RO(i) ; CnoisTilde(cnoisTilde) = i;  
        CnoisTildeAux(cnoisTilde)= Aaux(i ) ;   F(i) = 14;         
elseif   ( C(i) == '|' )  artif = artif + 1; Cf(artif) =  i ;  CfAux(artif) = Aaux(i ) ;
  Cfsubtyp( artif)=  Asubtyp(i, :) ;  F(i) = 16 ;
elseif   ( C(i)== '+' )   rhycp= rhycp+1;
CPLUS( rhycp) = Aaux(i) ; CPLUST( rhycp )=  Asubtyp(i, :) ; 
CPLUSi{ rhycp }=  Aaux( i , : ) ; F(i) =28;
if ( Aaux( i , 2) == 'N' ) ,  rm=rm +1; Cn(rm) = RO(i);  F2(i) =28;
elseif ( Aaux(i,2)== 'B' ) cbn = cbn +1;   Cbbb(cbn) =RO(i); F2(i)=28;
  elseif (Aaux(i,2)== 'V') cvt=cvt +1;Cvtc(cvt) =RO(i); F2(i)=28; %tachycardia
 elseif (Aaux(i,2)=='T') cvt=cvt +1;  Cvtc(cvt)= RO(i); F2(i)=28;%tachycard
  else      rhyc = rhyc+1;  F(i) =28;
 end;  if length(  Aaux(i ,: )) >=3  ||  Afilemit == 234 ,
 if ( Aaux(i ,3) == 'O' && Aaux(i ,4) == 'D'), 
     cnodal=cnodal+1; Cnod(cnodal)=i;  else  cnodal172= cnodal172 +1; 
 end;    else  cnodal172= cnodal172 +1; 
 end;  if (Aaux(i,1) == '(' ) rm2=rm2 +1; Cno(rm2) =RO(i); F(i)=28; end ;
 elseif (C(i)== '"'), if (Aaux(i,2)== 'N'),m3=m3 +1; Cnq(m3)=RO(i); F(i) =22;
 else   F(i) =22;  end; 
 %elseif (C(i)== 'n')  cpvc=cpvc+1; Rpvc(cpvc)=RO(i);F(i).C= 5; %vpc
%103 106    %x Non-conducted P-wave (blocked APC)
  elseif (C(i)== 'x' ),  cnpx =cnpx +1; CNPX(cnpx)= i ;   
 CNPXSubtyp(cnpx)=Asubtyp(i, :) ; CNPXAux(cnpx)=Aaux(i);F(i)= 37;      
elseif    (C(i)== '!'),  exclam = exclam +1; CExclam(exclam)= i ; 
CExclamR(exclam)=RO(i);    F(i)=31 ; % F(i).C=  13;  A.C(i) = 'Q' ; 
elseif    (C(i)== '{'), 
 flutterl = flutterl +1; Cflutter(flutterl)=i ;Cflutterl(flutterl)=RO(i); F(i) = 32;  
elseif  (C(i)== '}'),  
 flutter =flutter +1; Cflutter(flutter)=i ; CflutterR(flutter)=RO(i); F(i) = 33;
else  e = e +1; CElse(e)=i ;  CElseR(e)=RO(i); F(i) =22;      
end;    c(i) = CO(i) ; FR(i) =RO(i);   if  0,  disp(F(i));  end;  
end ; if 10,  [ cor  cc] = size(CO) ;   uc=unique( c )
for i = 1: length(uc)   CLL{i} =find( c == uc(i)) ; Class( i)= length( CLL{i}) ;
fprintf('176 i=%d c=%d= %s Len=%d \n', i, uc(i) , CLASS{uc(i)}, Class(i));
end; if ( cc >1) ,  uC = unique(C) ; else uC=unique(C') 
end;   RO =  double(RO) ; order =2 ; %ds=datestr(now) ;   
fprintf('Line 176  Afilemit=%d %s\n', Afilemit,uC ) ;% step removal of annot
fprintf(' 176  uANNCO=%s file =%d \n', uANNCO  , Afilemit ); uANNCO'
for i = 1: length(uANNCO)
COCL{i} =  find ( C == uANNCO(i) ) ;  COclass( i ) = length( COCL{i}) ;
    fprintf(' 181 i=%d uANNCO=%c Len =%d \n', i, uANNCO(i) , COclass(i));
end;   [ cnp]= find (c ~=  28  &  c ~=  31 &  c ~=  32  &  c ~=  33 )   ;    
[ cnpc]= find ( C ~= '+'  &  C ~=  '!'  &  C ~=  '{'  &  C ~=  '}' ) ;  length(RO)
[ cnpcn]= find ( C == '+'  |  C ==  '!'  |  C ==  '{'  |  C ==  '}'  ) ;  
B1= find( RO>start & RO<NS ) ; %C=Annot.anntyp(B1);%whos -file FILE;
Annot= length(RO)~= length(CO)
A3=[]; for i=1: length(CO) row=[ RO(i) CO(i)]; A3=[A3 ; row]; end;

CA4 = C(cnpc); RA4 = single( RO(cnpc) ) ;  ca4= c(cnpc) ;
[ cnptc]= find ( CA4  ~=  '~') ;  [ cnptcn] = find ( C'   ==  '~');
cnpct= cnptc;  CA3=CA4 (cnptc) ; RA3= RA4 (cnptc) ;  ca3= ca4(cnptc);
[ cnptoc]= find ( CA3 ~=  '|') ;   [ cnptocn]= find ( C' ==  '|');
cnpcto=cnptoc; CA2= CA3(cnptoc); RA2= RA3(cnptoc);ca2= ca3(cnptoc);
[ cnptoqc]= find ( CA2 ~=  '"') ; cnpctoq = cnptoqc ;
CA1= CA2(cnptoqc) ; RA1 = RA2(cnptoqc) ; ca1 = ca2(cnptoqc) ;
[ cnptoqxc]= find ( CA1 ~='x'); [ cnptoqxcn]=find( CA1' ==  'x') ;
cnpctoqx = cnptoqxc; CA= CA1(cnptoqxc) ; RA = RA1(cnptoqxc) ; 
cA = ca1(cnptoqxc); %if Afilemit == 220, CA(1)=[]; cA(1)=[];RA(1)=[];  end;
 Ca = CA; Ra = RA;  ca = cA; uca =unique( ca ); 
if 0&& Afilemit ==222,  [ fj  ] = find( ca ==8) ;[CPLUS] = find(ca == 11) ; 
 ca( fj) = 11;  Ca( fj)  = 'j' ; ca( CPLUS )  = 8; Ca( CPLUS)  = 'A';
 end; %  if Afilemit == 118 || ( Afilemit == 201 ) || (Afilemit == 219 ) 
  % Ra=RA1;  Ca= CA1 ;  ca = ca1;  end;
 if Afilemit > 400,  Ra=RO;   end;  
 [ ACAS corc ] = size(Ca) ; sca0 =  size( ca, 2);  sca1= size(ca1,2 ) ;  
  fprintf(' 205  sca0 %d   sca0ca1 %d %d \n', sca0, sca1,  ACAS ) ;    
 [ ACAPC ]= find ( ( ca ==4 ) | ( ca ==7)|( ca == 8 ) | ( ca == 9 ) )  ;
  [ CRBB ]= find ( ca ==3);  AAP= find( ca== 4);AAS= find( ca ==9 ) ;  
 [ ACPVC]= find( (ca ==5 ) |(ca == 10) );DUR= find( ca==11) ;
 [CAFusion]= find (Ca == 'F' );  [CaFusion]= find (ca == 6 ) ; 
[CNca]= find( ca==1);  [CaQ ]= find (Ca == 'Q' ); 
[ CPVCV ]= find ( ca ==5 ) ; [ CPVCE  ]= find ( ca == 10 ) ; 
 [ ACAPC9]= find(ca==9) ;  ACAPC9R = Ra(ACAPC9) ; 
 [ COTHER]= find ( (CO ==14 ) |( CO ==28)| (CO == 37) ) ;
  [DB2 ]= find(Ra>=start & Ra <= DN) ; DRT =Ra(DB2);  
 RPSTP=find( ca == 4  |  ca == 7 |  ca == 8  |  ca == 9 );  
  zlensort1820I= find(ca == 1 |  ca == 2 | ca == 3 | ca == 11 | ca ==34);  
  AA3=[];  [ AA3 ]= find( ca ==4) ; if 0&DBUG, dbstop 222; end;
  for i = 1: length(uca)  CaCL{i} =find( ca== uca(i) ); 
    Caclass( i ) = length( CaCL{i} ) ;
    AA1(i) =  find( ca == uca(i) ,1 , 'first') ; CaAA1= Ca(AA1(i))';   
    AA2(i) =  find ( ca == uca(i) ,1 , 'last') ; CaAA2 =Ca(AA2(i));
fprintf('220 %d ca%6d len=%4d ca=%c AA1%6d\n',...
    i,uca(i),Caclass(i),CaAA1,AA1(i));
  end; if ~BL&0,   dbstop 229 ; end;
  if Afilemit == 111, AA3=find( ca== 4); AA =unique([ AA3 1313 1 349 472]')' ;
  elseif Afilemit == 113,   AA3= find( ca== 4); AA = unique( [ AA3 1313 ]' )' ;
  elseif Afilemit == 114; AA3 =   unique( [AA1   AA2  ]' )' ;
 elseif Afilemit == 117,[ AA3 ]= [ 615 ]  ;   [AA3 ]= find (Ca == 'A' );  
 elseif Afilemit == 121,[ AA3 ]= [ 1017 ]  ;    
  elseif Afilemit ==124, AA3= find( ca==7);  AA=unique( [AA3  RPSTP]')' ;
   elseif Afilemit == 200,[ AA3 ]= [ 1558 ]  ;  
  elseif Afilemit == 201, AA3= unique([RPSTP 449 743 DUR]' )';  
      AA=unique( [AA1  AA2 449 704 1609 AA3 DUR ]' )' ;
  elseif Afilemit == 202,  AA3= unique( [AA3 1103 1105 ]' )' ;   
  elseif Afilemit == 203, [ AA3 ]= find ( ca ==5 ) ;  
  elseif Afilemit == 207; AA= unique( [AA1  AA2 1655 1647 1651 1749 1752]' ) ;
  AA3 =unique( [ AA1  AA2  CPVCE 1646  1647 1651 1749 1752 ]' )' ; 
  AA  = unique( [  1655 1647 1651 1749 1752]' ) ;
  elseif Afilemit == 208, AA3=unique( [AA1  AA2 2 3 126 159  1770 1774 ]')';
 AA3 =unique( [CPVCV(1:10)  2 126 ]')' ;
AA3= [RPSTP 2  3  126 159 1770 1774 CaFusion ] ;    
  AA= unique( [ AA1  AA2 AA3  [ 1770 1774 ] ]' )' ; 
    AA =setdiff([  AA  ]  , [ 35  65  78 87  ]')';% AA3= find(ca== 8); 
  elseif Afilemit == 209, AA3= [AA3  126 159  158  510 689 ] ;
   elseif Afilemit == 210,   AA3 = find( ca== 4), 
  elseif Afilemit == 212,  AA3= unique( [RPSTP 1 ]' )' ;      
     elseif Afilemit == 213,  AA3=[ unique( [RPSTP CaFusion ]' )' ]  ; 
 elseif Afilemit == 214,  AA3= unique( [RPSTP 1453 ]'  )' ;      
  elseif Afilemit == 220,  AA3= unique( [RPSTP  ]' )' ;%AA3=find(ca==34);
  elseif Afilemit == 222,  AA3= unique( [673  801  DUR 675 ]' )' ;   
elseif Afilemit== 223, AA3= [ AA3 83  86 162 306 1307 1668]; 
  AA = setdiff([   86 191],[162]);% AA3=find(ca==8);  
  elseif Afilemit == 232; AA3 =   unique( [AA1   AA2  ]' )' ;
     elseif Afilemit == 233; AA3 =   unique( [AA1   AA2 27 ]' )' ;   
  elseif Afilemit == 234, AA3= find( ca== 7); AA =unique( [ AA3 1303]' )' ;
  else AA3= find( ca== 4);  
  end;  AA= unique( [ AA1  AA2 AA3  ]' )' ; 
if Afilemit == 208,   AA =setdiff([  AA  ]  , [ 35  65  78 87  ]')';
   elseif Afilemit == 222, AA = setdiff([  AA ]  , [ 653  ]')';
 elseif Afilemit == 202, AA = setdiff([  AA ]  , [ 680  ]')';           
  elseif Afilemit == 207, AA = setdiff([  AA  1647 1655 ], [238  1653  ]')';          
  end;  
end;end; if 18, if 1,  AA4= unique( [ AA3  AA1 AA2  ]' )' ; 
if  size( AA ,1) ~=1, AA= AA'; end; 
AA= unique( [AA  AA1 AA2 ]' )' ; CU=length(AA);CU=6;
  if length(AA) >CU, AA =unique( [  AA(1:CU) ]' )' ;  end;
  R1000 =Ra(AA) ; resolution=  11;  bit11=  100/(2^11);   steps=  2^11
  zerovalue = Header.adczero;  gain =Header.gain ; color = 'b' ;
 DN =3*360; Ra =double(Ra ) ;  ecgnr=Afilemit ; clear F2 FR  cnpc* cnpt* ; 
 clear h  Rtime ecg xd  FILEMIT  FILEMITD  ANNCONPT ANNTOTAL  ;
 clear cnp cnpc cnptc ANN ANNCONP  anconpto  CNB; 
 string1=['1. ECG data of 3 second of Record   \color{blue} '  Afilemits ];
 CL=( [ string1  ] ); adczero1=Header.adczero(1);
zero=int2str( zerovalue(1) ) ;  Ax= 0.6;   Bx= 0.7; Ay= 0.07 ; By=  Ay; 
strlead=['1 ECG Channel 1   \color{magenta} ' ...
lead1 ' \color{red} MITBIH   \color{magenta}' Afilemits ] ;
string2=['2.Subtracted by ADCzerovalue ' ...
    'used in Digitization \color{blue}' zero ];
string3=['3 Deamplified by ADC' 'amplification factor gained in Digitization:' ...
  '\color{blue}' int2str( gain(1) )]; stringcell3 = { 'milli Volts'  ;  'Amplitude' ;} ; 
 min1 = min(ecg1(start:DN,1 ))  ; max1= max(ecg1(start:DN) ) 
min2=   min(x_a(start:DN,1 )) ; max2= max(x_a(start:DN,1 ));  
if  max2 > 0 ,fy2 =  [ min2  0   max2 ] ; else fy2 =  [ min2     max2 ] ;
end; for i= 1: length( fy2 )  [ ST2{ i,1} , errmsg] = sprintf('%5d',  fy2(i) ) ; 
end;CELL2=[0:1:3]; mind1=min(xd1(start:DN)); maxd1=max(xd1(start:DN)); 
if (0<maxd1), CELL3y=[mind1 0  maxd1]; else CELL3y =[mind1  maxd1];end;
 for i=1: length(CELL3y) [ ST3adc{ i,1}, cT]=sprintf('%5.2f', CELL3y(i));end;   
AMP_S = 0:1080/3:1086
 DB=find(Ra>start & Ra<=DN); DRB=Ra(DB); [s1 s2]=size( CO);
CA2=[ 'Reconstruct ' Afilemits , lead1 ' Channel  ' num2str(Caclass) ] ;
end;  if 10, hadcgain = figure( 'Name', [ '300'  CA2 ] , ...
'Position',[ 0 0 scr(3)/2-10 scr(4)/2-10]);h=gcf;set(h,'WindowStyle','docked');
hsub1= subplot(3,1,1);  fy1=[ min1 adczero1 max1 ] ; 
if max1<adczero1, fy1=[ min1  max1]; 
end; if  Afilemit==109, fy1=[min1 max1]; end; 
   set(gca,   'YTick', fy1 , 'Units', 'normalized', 'FontUnits', 'points', ...
  'FontWeight','normal',  'FontSize', fonts16, 'FontName','Times');   
  plot( start: DN , ecg1( start:DN )  ,  color , 'LineWidth', 2 );   
xlabel( 'Time in samples, 360 samples  per second',...
    'Color', 'b' , 'FontSize', fonts16 );  hold on;  
 ylabel( stringcell3{1} , 'Color', 'b' ,   'FontSize', fonts16 ); 
 ylabel('Amplitude, mV' ,  'FontSize', fonts16  , 'Color', 'b'  );  
 title([ string1] ,'Color','r', 'FontSize', fonts16); set( gca,'XTick',0:1080/3:1086); 
 set( gca,'XTickLabel', { 0:1080/3:1086} ) ;  xlim([1  1084]) ; 
if ( max1< adczero1) fy1=[ min1  max1 ]; else fy1=[ min1 adczero1 max1];end;
 set( gca,  'YTick', fy1 ) ; 
  for i= 1: length( fy1 ) [ ST1{ i,1} , errmsg] = sprintf('%5d',  fy1(i) );  end;
  set( gca,  'YTickLabel',  ST1 );  ylim([ min1-10 max1+10  ]) ;
hsub2=subplot(312); min2=min(x_a(start:DN,1));max2=max(x_a(start:DN,1));  
if  max2> 0,fy2 =  [ min2  0   max2 ] ; else fy2 =  [ min2     max2 ] ;end;
  set(gca,  'Units', 'normalized', 'YTick', fy2 ,  'FontUnits', 'points', ...
 'FontWeight','normal',  'FontSize', fonts16 , 'FontName','Times') ;   
plot( TIM,  x_a(TIM , 1) , color , 'LineWidth', 2  ); hold on; 
 CELL2 =  [ 0 :1:3 ]  ;  fy2 = [ min2 0 max2 ] ;  
 set(gca,'XTick', AMP_S ) ;  set(gca,'XTickLabel', CELL2);  
 ylabel( stringcell3{1} , 'Color', 'b' , 'FontSize', fonts16) ; 
ylabel('Amplitude, mV' ,  'FontSize', fonts16  , 'Color', 'b'  );  
xlabel('Time in seconds, 360 samples per second' ,'Color', 'b', 'FontSize',16);  
 title( [ string2]  ,'Color', 'r' , 'FontSize', fonts16 ); xlim([1 1084]) ;
 if  max2 >0 ,  fy2 =  [ min2  0   max2 ] ; else fy2 =  [ min2  max2 ] ;
end;  set(gca,'YTick',  fy2) ; 
for i= 1: length( fy2 )  [ ST2{ i,1} , errmsg] = sprintf('%5d',  fy2(i) ) ; end; 
  set(gca,'YTickLabel', ST2 );  ylim([ min2-10 max2+10  ]) ;
if 0, for k=1: length( DRT)  Aaux=[  num2str(Ra(k))  ];
if  Ra(k)>0, text( Ra(k) , ecg1( Ra(k)) , Aaux  ,'FontSize', 20  );  end;
 end;   end;   hsub3 = subplot(3,1,3); 
 set( gca,  'Units', 'normalized', 'YTick', CELL3y,   'XTick', 0:1:3 , ...
 'FontUnits', 'points', 'FontWeight','normal',  ...
     'FontSize', fonts16, 'FontName','Times') ; 
 plot(  xd1(TIM), color , 'LineWidth', 2  ) ;  title( string3 , 'Color','r',  ...
     'FontSize', fonts16);   cnptoqc= [ Afilemits 'DeamplifiedbyADCgain'  ] ;  
 set(gca,  'XTick', 360:360:1080);  set(gca,'XTickLabel',  [ 1:1:3 ]); 
xlim([1  1086]) ; set(gca,'YTick', CELL3y  ) ; ylim([ mind1  maxd1  ]) ; 
  set(gca, 'YTickLabel',  ST3adc   ) ;  Cnorm =   {   'milli Volts' ;  }  ;
  ylabel(    Cnorm  , 'Color', 'b'   , 'FontSize' , fonts16    ) ;
  xlabel(' Time in seconds' , 'Color', 'b' , 'FontSize' , fonts16   );  
ylabel( 'Amplitude, mV'  ,   'FontSize', fonts16  , 'Color', 'b'  );
har= annotation( 'textarrow',  [Ax  Bx ] ,  [ Ay  By ]  )  ;
 if 0, line( [ 0,1000] , [0,0] , 'Marker' ,   '.'  , 'Color', 'b'  );  end;    
 % % % if start==1  Ra(1)= []; C(1)=[];  B1(1)=[];end;
 if  Afilemit < 400,    for k=1:length(DRT)     string19{k}=[  (Ca(k))  ];
if  Ra(k)>0,text(  Ra(k) ,   xd1( Ra(k) ) , Ca(k)  ,'color','b' );  end; end;
  else    for k=1: length(DRT)     string195{k,1}= [ num2str( Ra(k))] ;
text(  Ra(k) ,  xd1( Ra(k) ) ,  [ num2str( Ra(k))]  ,'color', 'b'   ); end; 
 end;  cnptoqc= ['AmplifiedbyADCgain' Afilemits ] ;d=datestr(now) ; 
dates=[ d(10:11),d(4:6), d( 1:2)  d(13:14), d(16:17) ]
 FIGFILE= [ cnptoqc d(1:2), d(4:6), d(10:11), d(13:14), d(16:17) B ];
  FIGUFILE = fullfile(PATFIG, FIGFILE) ; if DBUG& Afilemit==233 
saveas(  hadcgain , [ FIGUFILE  Afilemits  B  'png' ] , 'png')  ;  
end; end; if 1, CA3 =[ ' ADC ' Afilemits , lead1 ' Channel  '  ] ;
 hadc = figure( 'Name', [ ' 360 '  CA3 num2str(Caclass) ] ,'Position', ...
 [20  2  screen(3)/1-40 screen(4)/1-40]); h=gcf; set(h,'WindowStyle','docked'); 
 set(gca,  'Units', 'normalized', 'FontUnits', 'points', ...
 'FontWeight','normal',  'FontSize', fonts16, 'FontName', 'Times' ); 
hasub1= subplot(3,1,1); set(gca,  'Units', 'normalized', 'FontUnits', 'points', ...
 'FontWeight', 'normal',  'FontSize' , fonts16, 'FontName', 'Times' ); 
plot(  xd1(TIM)  , color , 'LineWidth', 2  ) ; 
stringa1=['1. ECG of 3 sec of MITBIH  Rec    \color{blue} '  Afilemits];
stringa3=['3. Added by ADCzerovalue  in Digitization: \color{blue} ' zero];
stringa2 = ['2 Amplified by ADC  factor  in Digitization: ' ...
'\color{blue}' int2str( gain(1) )]; stringcell3 = { 'milli Volts' ;  'Amplitude' ; } ;   
  title( stringa1 , 'Color','r'  ,   'FontSize', fonts16 );  
set(gca,  'Units', 'normalized', 'FontUnits', 'points', ...
  'FontWeight','normal',  'FontSize', fonts16 , 'FontName', 'Times');     
  Ax= 0.6;   Bx= 0.7; Ay =   0.07 ; By =Ay; xlim([1  1086]);
set(gca,'XTick', 360:360:1080); set(gca,'XTickLabel', [ 1:1:3 ]); 
filsig2= 'Time in seconds,360 samples per second' ;
 xlabel(filsig2, 'FontSize',18, 'Color', 'b'); ylim([ mind1  maxd1]) ; 
  for i=1: length(CELL3y) [ ST3adc{ i,1}, cT]=sprintf('%5.2f', CELL3y(i));end;   
 set(gca,'YTick', CELL3y) ; set( gca ,'YTickLabel',  ST3adc ) ;
ylabel( stringcell3{1} , 'Color', 'b' ,   'FontSize', 18  );   
hasub2 = subplot(312) ; % if DBUG,   dbstop 370; end; 
set(gca,  'Units', 'normalized', 'FontUnits', 'points', ...
  'FontWeight','normal',  'FontSize', fonts16, 'FontName','Times');   
plot( TIM,  x_a(TIM , 1) , color , 'LineWidth', 2  ); hold on; 
set(gca,'XTick', AMP_S ) ;  set(gca,'XTickLabel', CELL2 ) ;   
 ylabel('Amplitude'  , 'Color', 'b',   'FontSize', fonts16); xlim([1  1084]) ;
 ylabel( stringcell3{1}   , 'Color', 'b',   'FontSize', fonts16 ); 
 xlabel( 'Time in seconds,360 samples per second' ,'Color', 'b' ...
 ,'FontSize', fonts16);  ylim([ min2-10 max2+10]) ;
 title( [ stringa2] ,'Color', 'r','FontSize', fonts16 ); 
if  0<max2 , fy2=  [ min2 0 max2 ] ; else  fy2=  [ min2   max2 ] ; end;
set(gca,'YTick', fy2 );set( gca, 'YTickLabel', ST2); hasub3 =subplot(3,1,3) ;
 set(gca,  'Units', 'normalized',    'FontUnits', 'points', ...
  'FontWeight','normal',  'FontSize', fonts16, 'FontName','Times'); 
plot(start: DN , ecg1( start:DN ) ,  color , 'LineWidth', 2 ); ecgnr= Afilemits ; 
 xlabel('Time in samples, 360 samples per second',...
 'Color', 'b', 'FontSize', fonts16);  title([stringa3] ,'Color','r','FontSize',16); 
 ylabel( stringcell3{1} , 'Color', 'b' , 'FontSize', fonts16  );   
set( gca,'XTick', 0:1080/3:1086); set( gca,'XTickLabel', { 0:1080/3:1086}) ; 
if adczero1< max1,YTick= [ min1 1024 max1 ]; else YTick=[min1  max1]; end; 
set(gca,  'YTick', YTick) ;  ylim([min1  max1]) ; set( gca,'YTickLabel', YTick );   
cnptoqc = [  'DeamplifiedbyADCgainadc' Afilemits] ; xlim( [1  1084]) ;   
FIGFILE= [ cnptoqc  d(1:2), d(4:6), d(10:11), d(13:14), d(16:17) B ] ;
FIGUFILE =fullfile(PATFIG, FIGFILE); if 1 | (DBUG& Afilemit==233),; 
     saveas(  hadc  , [ FIGUFILE Afilemits  B  'png' ] , 'png')  ;  
 end; end;  if 1, anot= 'atr';  Krr = 5 ;  [ clx CNPXV ]=find(C== 'x' );
l1= ~isspace(lead1); lead1e5 = find(  lead1~=' ',5,  'last' ) ;  
l2= isspace(lead2);lead2ss = sscanf(lead2, '%4c',4) 
if isequal(lead1, ' MLII' ), 
[ lead1e2  lead1eI ]= find(~isequal(lead1,' ' ),5, 'last' ) ; 
lead1s= lead1( lead1e2+1: end );  
elseif 1     lead1e = find(  lead1~=' ',2,  'last' ) ; 
[ lead1e2  lead1eI ] = find(~isequal(lead1,' ' ),2,'last' ) ; 
lead1s= lead1( lead1e2+1: lead1e(1)-1) 
end; if isequal(lead2, ' MLII' )    lead2e = find(  lead2~=' ',5, 'last' ) ;
 [ lead2e2  lead2eI ] = find(~isequal(lead2,' ' ),5, 'last' ) ; 
lead2s= lead2( lead2e2+1: end ); 
elseif isequal(lead2, ' V5  ' ) | isequal(lead2, ' V1  ' ) | 1
 lead2e = find(  lead2~=' ',2,  'last' ) ; 
[ lead2e2  lead2eI ] = find(~isequal(lead2,' ' ),2,  'last' ) ; 
lead2s= lead2( lead2e2+1: lead2e(1)-1);  
else end; res = 0;
ti = '1';  tf = int2str(NS) ; Kq = 1.5; Kr = 5;  Ks = 3;  
 Kpb = 1.35 ;  Kpe = 2; Ktb = 2; Kte = 3.5;   pco= 8;  typerec = 0 ; 
dirhea = Pmit; dirsig = Pmit; dirann = Pmit;  nbo_flag= 0; 
addpath( genpath('D:\data\mitdb\featuref4\ecgpuwave') ); 
string=[uC num2str(ACAS) ' ' Afilemits ' ' num2str(Caclass) 'Channel ' lead2] ;
mind2= min( xd2(start:DN) ) ;  maxd2=  max( xd1(start:DN) ) ;  
 if  size(xd2 ,1) >1081, z1= 1081; z2= 2160; else z1=1; z2=1080; end;   
uCh =unique( C' );  uCa =unique( Ca' );  [ udiff, id ] = setdiff(uCh, uCa ) 
if Afilemit ==208, hi= 0.2; else hi=0.2; end;  [clpv]= find( C == '+'); 
[ARCN]=find( C == '~' ); [clx CNPXV ]=find( C== 'x' ); 
 RTilde =  (length(ARCN)') ;  RTildes =  int2str(RTilde)
 for i=1 : RTilde-1  ARCNd(i)=ARCN(i+1)-ARCN(i ); end;

end; if RTilde>=1, [ ARCNdmin  ARCNdI ] =min(ARCNd);
 [ ARCNso  ARCNdsoI ]=sort( ARCNd); 
if ( Afilemit == 201), v1= ARCN(1);  w1=ARCN(4 ) ; v2r=[ v1 : w1  ]
elseif ( Afilemit == 207), v1= ARCN(1); w1=ARCN(4 ) ; 
v1= ARCN(ARCNdI) ;   w1= ARCN(ARCNdI+1) ;  v2r=[ v1 : w1 ]
elseif ( Afilemit == 208), v1= ARCN(1); w1=ARCN(4 );  nhi=-0.2; hi=-0.2; 
  v1= ARCN(ARCNdI) ;   w1= ARCN(ARCNdI+1) ; v2r=[ v1 : w1  ]
 else  v1= ARCN(ARCNdI) ; w1= ARCN(ARCNdI+1) ; v2r=[ v1 : w1  ]
 end;  u1= v1-1;  U= RO(v1)-300; V=RO(w1+1) +300;
end;  if RTilde>=1, h2noise=figure( 'Name',[Afilemits 'h2noise 440' RTildes] , ...
'Position', [ 20 -40  screen(3)-20 screen(4)/1-40 ]  ); 
if U<0, U=1; end;h=gcf; set(h,'WindowStyle','docked'); 
h2nois=get( gcf , 'CurrentAxes' );  % set( h2noise, 'CurrentAxes', h2nois );
 plot( U : V, xd1( U : V ) , cb , 'LineWidth', 2  ); %thesis 
mn2 =min(xd1(U:V ))-0.2; mx2 = max( xd1(U:V ))+3*0.2;  
fyTilde = ( mn2:( mx2- mn2 )/2: mx2 ) ;  set( gca, 'YTick', fyTilde ) ; 
set( gca,  'Units', 'normalized',  'FontUnits', 'points', 'FontWeight', 'normal', ...
 'FontSize', fonts16, 'FontName','Times' ) ;
if 0, for v2= v2r  ROM=[ int2str(RO( v2)) ] ; 
text(RO(v2),xd1(RO(v2))+2*hi,[int2str(v2) C(v2) ROM],'color', cr,'FontSize',16);
end; end; if 10,   text( RO( u1 ), xd1( RO( u1 ))+ 2*hi , ...
[ '\downarrow' int2str(u1)  C(u1) int2str(RO( u1))],'color',cb, 'FontSize' ,16);
 text( RO( w1+1 ),  xd1(RO( w1+1))+ hi , ...
['\downarrow' int2str(w1+1) C(w1+1) int2str(RO(w1+1))],'color',cb,'FontSize',16); 
ylim( [ mn2   mx2 ] ) ;   xlim( [U  V] ) ; 
set( gca, 'XTick', U: fix((V-U)/2):V) ; set( gca, 'XTickLabel', U:fix((V-U)/2):V) ;
 end; ylabel('Amplitude, mV'  ,'FontSize',  16  , 'Color', 'b'  );
for i=1: length( fyTilde ) [ ST1{i,1} , errsg] =sprintf( '%.2f', fyTilde(i) ); end;
 set( gca ,'YTickLabel' ,  ST1);  
 ylabel( 'Amplitude, mV'  ,   'FontSize',  16  , 'Color', 'b'  );
xlabel( 'Time in samples, 360 samples  per second', ...
 'Color' , 'b' , 'FontSize' , 16 ); ylim( [ mn2  mx2 ] );  xlim([ U  V]);  
if 10, text( RO(v1 ), xd1( RO(v1 ))+2*hi ,...
[ '\downarrow' int2str( v1 ) C(v1) int2str( RO(v1))], 'color', cr,'FontSize',16); 
 text( RO(v1+1), xd1( RO(v1+1 ))+hi ,...
['\downarrow' int2str(v1+1) C( v1+1) int2str(RO(v1+1))] , ...
 'color', cr , 'FontSize', 16 ); 
end; if w1~=v1 &&w1~=v1+1,     text( RO( w1 ),  xd1(RO( w1 ))+2*hi  , ...
['\uparrow' int2str(w1) C(w1) int2str(RO( w1)) ], 'color', cr , 'FontSize',16); 
end; title([ 'Noise Annotations in Class label list of  Record  ' Afilemits], ...
'color' , 'r' ,  'FontSize' ,16 ) ;  cnptoqc = [   'h2noise' Afilemits] ; 
 FIGFILE= [ cnptoqc   d(1:2), d(4:6), d(10:11), d(13:14), d(16:17) B ] ;
 FIGUFILE = fullfile( PATFIG, FIGFILE )  ; if  DBUG,
 saveas(  h2noise  , [ FIGUFILE Afilemits  B  'png'] , 'png' );  
end; end;  end; if LAGUNA,   limits_141022 ;
 ANNT= annt; ANNTR= ANNT.R;  AFR= annt.fiducial ; QQM= ANNT.Q ;
 QM = ANNT.QRSonset;  SM2= ANNT.S;
SMnanN2 = find(~isnan(SM2) ); S( SMnanN2) =SM2( SMnanN2 );
SM = ANNT.QRSoffset;  SMnanN =  find(~isnan( SM ) )  ;
TPM =ANNT.T; TPMnanN = find(~isnan(TPM) );no_beats2= length(ANNTR);
[ uRO  uROI ] =unique(ANNTR);A60 = setdiff([1: length(ANNTR) ], uROI );
ROM60 = ANNTR( uROI ) ;  CL60= CL(uROI ); uCL60 = unique( CL60) ;
ANNTR=ANNTR-3;  [Era Erai] =  setdiff ( Ra, ANNTR') ;
[ inters , ia, ib ] = intersect(Ra, ANNTR');
if ACAS ~= no_beats2,   if 0&&DBUG dbstop 485; end; 
 if  Afilemit == 213 || Afilemit ==100 || Afilemit == 113 || Afilemit == 220 ...
||  Afilemit == 116 ||  Afilemit == 210 || Afilemit == 212 || Afilemit == 233  ...
||  Afilemit == 115 
 else fprintf('491 f=%d pe=%d\n', ACAS, no_beats2) ;    
 return; end;  end;    end;  if 1,  clear( [ 'ecg' Afilemits ] ) ;
time=(1: 1080); clear  RTD Annot rvh ANNtime ;   clear   aux2 aul  B1 ; 
clear CnoisTildeR ;  clear( [ 'C' Afilemits] );   clear ecg_d  CHANFIELD  ;    
clear annconp annconpt   ANNCO  ROCNP  ANNTOTALAux CNAME  ;
clear  AFR   Annot ANNTOTAL  AC  AR    QRS  HEAD ANN  ecg ecg1 ;
clear rvd RTDPM RTDP   ANNTR    ANNCONP   ANNRO  rvh RTL RTR ;   
clear  ANNCONPT Cd  CONORM  CNca CPace*  CPLUS* ;wCa =length(Ca'); 
clear  QT  T  SUBTYPEFIELD X Xpa Xpb   annot annt    AMP_Q AMP_R ;
clear ecg1 ANNOT AMP  FILEMIT  FILEMITD    fm1 fm2 filsig fy ecg1; 
clear  DBT  POS_*    Aaux  DM Der  FM  NUMFIELD ; ds=datestr(now);   
clear  CA4   CO32 RA4  COM  Cnorm; clear  CLL C_AB   cl cla  uRO uROI  ...
A67 Rac RacI ca4 ca3 ca2 Rpvc Cbbl Racl  VAL* POS_ANNOT;
DRB=DRB-start+1; Range =Ra-start+1; [ ARC ] = find(CO' == 28);   
wARC=length(ARC);  uCh  =unique( C' );  uCa =unique( Ca' )     
A3=intersect ( uCh, uCa); [  diff, id ] = setdiff(uCh, uCa  ); wC=length(C);  
if Afilemit ==208, hi=0.2; else hi=0.2; end;  Asubtyp= length(ARC)
for i = 1 : wARC-1  ARCd(i)=ARC(i +1)-ARC(i ); end;
if  wARC>1, [ ARCdmin ARCdI] = min(ARCd );  end; order=2; 
stringw = [ 'Rhythm Changeover  ECG ' Afilemits ' Channel 1', lead1 ];
end; if   wARC>= 3, hrhythm = figure( 'Name', [ '510' stringw ] , ...
'Position', [ 10 10 screen(3)/1-100 screen(4)/1-110] ); 
h=gcf; set(h,'WindowStyle','docked'); 
v1= ARC(ARCdI); w1=ARC(ARCdI+1);   v1rc= v1; w1rc=w1;
for  i = 1 : 1  % length(ARC)-2 %  subplot( Asubtyp/2,2 , i );
sp(i) = subplot(  i , 1,  i ); %u1= ARC(i); v1= ARC(i+1 );  w1=  ARC(i+2 ); 
 ROv1= RO(v1-1);  ROw1=RO(w1+2 );V=RO(w1 )+300;
 U=RO(v1)-300;  V =  RO(w1+2 )+400 ;if U<0,U=1; end; 
 m= min(xd1(U : V))-0.2;  mx= max(xd1(U : V))+0.2; axis([ U ,V,  m, mx ] ); 
set( gca, 'Units' , 'normalized', 'FontUnits', 'points', ... 
'XTick', U:fix((V-U)/2):V ,  'FontUnits', 'points' , ... 
 'FontWeight', 'normal', 'FontSize', fonts16, 'FontName', 'Times' ) ; 
plot( U : V, xd1( U : V) , cb , 'LineWidth', 2 ); %thesis
if 0, line([ U, V], [ 0,0 ] , 'Marker' , '.', 'Color', 'b' , 'LineWidth', 2);  end; 
ylim([ m  mx ] ); xlim( [ U  V ] ) ; v2rc =[ v1  w1  ] ; 
if 0,  for v2= v2rc  ROM=[ C( v2)  int2str(RO( v2) ) ] ;  int2str(RO( v1))
text(RO(v2),xd1(RO( v2))-1*hi, [ int2str( v2) ROM ], 'color', cr,'FontSize',16);
end; end; if 10, text(  RO(v1) , xd1(RO( v1 ))-2*hi  ,...
[ '\uparrow' int2str(v1) C(v1)  ] , 'color' , 'r' , 'FontSize',16 ); 
text( RO(w1), xd1(RO( w1))+2*hi, ...
[ '\downarrow' int2str(w1) C(w1) ], 'color', 'r', 'FontSize', 16); int2str(RO(w1))
end; if 10, text(RO(v1-2), xd1(RO( v1-2))-0.51*hi , ...
[ int2str(v1-2) C(v1-2)  ], 'color', 'b'  , 'FontSize',16 );
text(RO(v1-1), xd1(RO( v1-1))-0.5*hi  , ...
 [ int2str(v1-1) C(v1-1)  ], 'color', 'b' , 'FontSize',16 );
text(RO(v1+1), xd1(RO(v1+1))+0.75*hi, ...
 [  int2str(v1+1) C(v1+1)   ], 'color','b'  , 'FontSize',16);
text(RO(w1+1), xd1(RO( w1+1))-0.5*hi , ...
    [ int2str(w1+1)  C(w1+1) ], 'color', 'b' , 'FontSize',16);
text(RO(w1+2), xd1(RO( w1+2))-0.5*hi ,  ...
[ int2str(w1+2) C(w1+2)  ], 'color', 'b' , 'FontSize',16 );
text(RO(w1+3), xd1(RO( w1+3))-0.5*hi, ...
    [ int2str(w1+3) C(w1+3)], 'color','b', 'FontSize',16 );
end;  fy25= ( m: (mx-m )/2: mx  ) ;  
set(gca, 'YTick'  , fy25  ) ;  ylim([ m  mx ] ) ;  xlim( [ U  V ] ) ; 
set( gca,'XTick', U : fix((V-U)/2) : V); set(gca,'XTickLabel', U:fix((V-U)/2):V);    
title( [ 'Rhythm change annotations in Class label list, Record ' Afilemits ], ...
'color' , 'r' ,  'FontSize' ,16 ) ;
for i= 1: length( fy25 ) [ ST1{ i,1}, errmsg] =sprintf( '%.2f' , fy25(i)); end;
 set( gca,  'YTickLabel',  ST1 );  
 ylabel('Amplitude, mV' ,  'FontSize', fonts16  , 'Color', 'b' );
xlabel('Time in samples, 360 samples  per second',...
 'Color', 'b' , 'FontSize', fonts16 );   cnptoqc = [  'hrhythm' Afilemits ] ;  
FIGFILE = [ cnptoqc d(1:2), d(4:6), d(10:11), d(13:14), d(16:17) B ];
FIGUFILrhy = fullfile(PATFIG, FIGFILE ) ; if  DBUG, 
 saveas(hrhythm , [  FIGUFILrhy Afilemits  B  'png' ] , 'png' ) ;    
 end; end;  end;  if  0, hnf_6 =figure('Name',  ' 505  Notch Filter ',...
 'Position' , [ SCR(3)/2 20  SCR(3)/2 SCR(4)/3] ); A3= U1:(V1- U1)/2:V1 ; 
subplot(2,1,1 );  if  exist('xd1', 'var')  [ xlf1 , fy ] = notchfilt2(xd1,360); end;
subplot(2,1,2 );   if  exist('xd2', 'var'),[ xlf2 , fy2 ] = notchfilt2(xd2,360); end;
[ minxbf1 ] =  min( xbl1( U1 : V1 ) ) ; [ maxxbf1   ]=max( xbl1( U1 : V1 ));
RA3 = [ minxbf1 0  maxxbf1  ] ;fy2=( minxbf1:(maxxbf1-minxbf1)/2:maxxbf1) ;   
set(gca, 'Units', 'normalized', 'YTick', minxbf1:(maxxbf1-minxbf1)/2:maxxbf1 ...
, 'XTick', A3, 'FontUnits',  'points'  ...  %,'Position', [ .15 .2 .75 .7 ] ...
,'FontWeight',  'normal',   'FontSize', 20 , 'FontName', 'Times' ) ;     
end;  if  exist('xd1', 'var') && ~exist('xlf1', 'var'), xlf1 = xd1; end; 
if ~exist('xlf2','var')  &&  exist('xd2', 'var'), xlf2 = xd2;  clear xd2;  end;
 if ( Afilemit ==100 ), cof1 = 0.1; cof2= 55 ; zerr =0.5;  
elseif ( Afilemit==101),cof1= 0.1;  cof2= 40; BP1=0.1; BP2=65; 
 elseif ( Afilemit == 103 )   cof1=0.1;  cof2 = 50 ;
  elseif ( Afilemit == 105 ), cof1 = 0.18 ; cof2= 45; BP1=0.1; BP2=65; 
  elseif (Afilemit == 106),cof1 = 0.5; cof2= 30;  BP1=1; BP2=30; 
  elseif (Afilemit == 107) ,  cof1 = 1.5; cof2 = 35;%cof2 = 20 ;
  elseif   (Afilemit == 108 ),  cof1 = 0.8 ; cof2 = 30 ;
  elseif  (Afilemit == 109 ) ,  cof1 = 1.5; cof2 = 35 ;
  elseif ( Afilemit == 111 ) , cof1 = 0.2; cof2= 60 ;
  elseif ( Afilemit == 113 ) ,    cof1 = 0.1 ; cof2= 60 ;        
elseif ( Afilemit ==114), cof1 =0.5 ; cof2= 50;
    elseif ( Afilemit == 116 ), cof1 = 0.2; cof2= 50 ;
  elseif ( Afilemit == 118 ) ,    cof1 = 0.1 ; cof2= 30 ;   
   elseif ( Afilemit == 119 ) ,    cof1 = 0.1 ; cof2= 60 ;        
elseif ( Afilemit == 121 ),cof1 = 0.1 ; cof2= 45 ;  BP1 = 0.1; BP2 = 40  ; 
  elseif ( Afilemit == 122 )   cof1 = 0.1 ; cof2= 65 ;  BP1 = 0.1; BP2 = 60  ; 
  elseif ( Afilemit == 123 )   cof1 = 0.1 ; cof2= 15 ;   BP1 = 0.1; BP2 = 10  ; 
  elseif ( Afilemit == 124 ) ,  cof1= 0.1 ;  cof2 = 60 ;
elseif (Afilemit == 200), cof1= 0.5;cof2 = 60;BP1 = 0.61; BP2 = 60;
  elseif ( Afilemit == 201),order=2; cof1=0.5; cof2= 60;BP1 =0.4; BP2=10; 
if Bthesis   cof1 = 0.1 ; cof2= 85 ;  BP1 = 0.1; BP2 = 80  ; end; 
  elseif (Afilemit == 202)  cof1 = 0.1; cof2= 95;BP1 =1; BP2= 30; 
elseif ( Afilemit ==203), cof1= 0.5;cof2= 60;BP1=1.1; BP2=60;
  elseif ( Afilemit == 205 )    cof1= 1  ;  cof2 =30 ;BP1 = 0.6; BP2 = 30  ; 
elseif ( Afilemit == 207), cof1 =0.8; cof2 =30;  
 if  isequal( B,'BLN')  cof1=1.0; cof2 = 20; end;BP1=0.1; BP2=60; 
elseif ( Afilemit == 208 )   cof1=0.1 ; cof2 = 80 ; BP1 = cof1 ; BP2=  cof2;   
elseif ( Afilemit == 209 )   cof1= 0.1;  cof2 = 65 ;BP1 = 1 ; BP2=  40; 
elseif ( Afilemit == 210 ) ,  cof1= 0.12 ;  cof2 =60;BP1=0.1; BP2=60; 
elseif ( Afilemit == 212 ) ,  cof1= 0.2 ;  cof2 = 45 ;cof2= 20.2 ; 
elseif ( Afilemit ==213), cof2 =85;cof1= 0.8; if  isequal(B,'BLN'),cof1=1.0;end;
elseif ( Afilemit == 214 ) ,  cof1= 0.5;   cof2 =65 ; cof1 = 0.15; cof2= 60 ;
elseif ( Afilemit == 215 ) ,  cof1= 0.2; cof2= 50;  BP1=0.92; BP2=40; 
  elseif ( Afilemit == 217 ) ,   cof1 = 1.5; cof2= 20 ;  BP1 =  0.4; BP2=40; 
elseif ( Afilemit == 219),cof1 = 0.2; cof2= 50 ; order=2;BP1=0.14; BP2=90;   
cof1 = 0.2; cof2= 50 ; order=2;BP1=0.14; BP2=90;   
elseif ( Afilemit == 220) ,cof1=0.5; cof2=50 ;     
 elseif ( Afilemit ==221),cof1 = 1.5; cof2= 75;BP1=0.1;BP2=100;
elseif  ( Afilemit == 222),cof1 =0.8; cof2=40; if Bthesis,cof1=0.1;cof2= 90;end; 
  elseif ( Afilemit == 223 ) , order=2;  cof1 = 0.2 ;   cof2= 40 ;
    elseif ( Afilemit == 228 ),cof1 = 0.2; cof2= 30 ; BP1 =0.4; BP2=40; 
    elseif ( Afilemit == 231)   cof1 = 0.9; cof2= 30 ; BP1 =0.4; BP2=30;  
  elseif ( Afilemit == 232)   cof1 =  1; cof2= 17; BP1 =  0.4; BP2=20;   
  elseif (Afilemit == 233),cof1=0.1 ; cof2= 40;  BP1=0.4; BP2=10;     
  elseif ( Afilemit == 234 )  cof1 = 0.2;   cof2= 45 ; 
else cof1= 0.2; cof2= 50;  BP1=0.2; BP2=50;order=2;  zerr= 0.5; 
end; end;if 1,if 10, if ~BF, cof1 =0.1; cof2=100;end; 
 if ~exist('BP1','var'),BP1 = cof1 ; BP2=  cof2; end;
star=1; DIS=1080; [DBT2 ]= find(Ra>=star & Ra<=DIS);DRT2=Ra(DBT2);   
 k= [ 'BandPass Filter ' num2str(cof1) ' : ' int2str(cof2)  'Record:' Afilemits ]; 
 min1180e = min(xlf1(1: 1080)); max1525e= max(xlf1(1: 1080)); 
end; if 10, hband1=figure( 'Name', [ '600 Channel 1 '  k ] ,'Position',  ...
[10 30 SCR(3)/2-30 SCR(4)/2-30]); h=gcf; set(h,'WindowStyle','docked'); 
if  BF, % dbstop 619; 
    [  xbf1,a1,b1] = butterbpass( xlf1, cof1, cof2, order, k, Afilemits ); 
    end;   if  ~exist('xbf1', 'var'), xbf1= xlf1;    end;       
plot( time , xlf1(time) ,'color', 'r'  , 'LineWidth', 2  ); %subplot(2,1,2); 
hold on; plot(time, xbf1( time) ,'color', 'b'   , 'LineWidth', 2  ); 
 for i=1:length(DRT2 )    string198{i,1}= [ CA(i)  num2str( Ra(i))  ] ;
text(  Ra(i)+2, xlf1(Ra(i)+2 ),   [ string198{i}] , 'color','r'   , 'FontSize',16 ); 
 end; line([ 0,1080], [0,0] , 'Marker' , '.', 'Color', 'b' , 'LineWidth', 2);
  min1180g2= min(xbf1(1: 1080) ); max1525g2= max(xbf1(1: 1080))+0.11; 
Annconpto=max(max1525g2,max1525e)+0.10; 
AMP_S=min(min1180e,min1180g2)-0.10; ylim([ AMP_S   Annconpto]) ;
 if  0< Annconpto&0> AMP_S ,  fy2= [ AMP_S 0  Annconpto ] ;
 else  fy2= [ AMP_S    Annconpto ] ; end; ylim([ AMP_S   Annconpto]) ;
 set(gca,  'Units',  'normalized', 'YTick', fy2 ,  'FontUnits', 'points', ...
 'FontWeight','normal',  'FontSize', 20 , 'FontName','Times') ;     
for i= 1: length( fy2 ) [ ST1{ i,1} , errmsg] = sprintf('%.2f',  fy2(i) )  ; end;
 set( gca,'YTick', fy2);set( gca,  'YTickLabel',  ST1 );   xlim([1  1084]) ;dates
 set(gca,'XTick', 360:1080/3:1080) ;   set(gca,'XTickLabel', 1:1:3) ;  
 ylabel(  'Amplitude, mV' , 'FontSize', fonts16  , 'Color', 'b' );
xlabel('Time in seconds', 'FontSize' ,19, 'Color', 'b'); BP1s=  num2str(BP1) 
title( k ,'color', 'r','FontSize',16); cnptoqc= [ 'bpass1'] ; 
FIGFILEb=['butters2' Afilemits B  dates  , '.png' ];
FIGUFILE= fullfile( PATFIG, FIGFILEb); if  DBUG, 
 saveas( hband1, [ FIGUFILE ]); end; 
k2 = [ ' Band Pass Filter '  (BP1s) ' : ' int2str(BP2)   ' Rec:' Afilemits  ] ;
end;  if 10, hband2 =figure('Name', [ ' 645 Channel 2' k2 ] , ...
 'Position', [SCR(3)/2-30 30  SCR(3)/2-130  SCR(4)/2-30]);
h = gcf ;  set(h,'WindowStyle','docked') ;      
plot(time, xlf2(time)-0.0 ,  'color', 'r'  , 'LineWidth', 2  );  hold on;  
if 0, if ( Afilemit==101), BP1=1.1;  BP2=40 ; 
elseif ( Afilemit==200), BP1=1.1;  BP2=40 ;  else BP1=1.1;  BP2=40 ;
end;end; if BF, [ xbf2, a1,b1]=butterbpass(xlf2,BP1, BP2,order, k2 ,Afilemits ) ;
 hold on;  plot(time, xbf2( time) ,  'color', 'g'   , 'LineWidth', 2  ); 
end; if  ~exist('xbf2', 'var'), xbf2= xlf2;   end;   
line([ 0,1080], [0,0] , 'Marker' , '.', 'Color', 'b' , 'LineWidth', 2);
min1180g2= min(xbf2(1: 1080));  max1525g2=max(xbf2(1:1080))+0.01; 
U= start; V=1080; max1525e= max(max(xlf2(U:V)),max(xbf2( U : V)))+0.1; 
min1180e=min( min(xlf2( U:V)) , min(xbf2( U:V) ))-0.1; 
if  max1525e>0 &&  min1180e<0, fy2=  [ min1180e  0  max1525e ] ;
else fy2 = [ min1180e  max1525e ];end; ylim( [ min1180e  max1525e]);  
  set(gca,  'Units', 'normalized',    'FontUnits', 'points', ...
 'FontWeight','normal',  'FontSize', 20 , 'FontName','Times') ; 
k2 = ['Bandpass Filter '  (BP1s) 'hz  to ' int2str(BP2)  'hz,  Record:' Afilemits] ;
 title( k2,'color', 'r', 'FontSize',16);   ~strcmp(comp,'PCWIN64')
for i= 1: length( fy2 ) [ ST1{ i,1} , errmsg] = sprintf('%.2f',  fy2(i) );end; 
set( gca,'YTick', fy2 ) ; set( gca,  'YTickLabel',  ST1); xlim([1  1084]) ;
set(gca,'XTick', 360:1080/3:1080); set(gca,'XTickLabel', 1:1:3);  
xlabel( 'Time in seconds ' ,'FontSize',19, 'Color', 'b' );
 ylabel('Amplitude (mV) ', 'FontSize', 18, 'Color', 'b');  
cnptoqc= [ 'bpass2'] ; ds=datestr(now) ; %title(string);  ECG=M(:,2);
dat=[  ds(1:2), ds(4:6), ds(10:11), ds(13:14), ds(16:17) ];
FIGF=fullfile(PATFIG, [ 'butters2' Afilemits B  dat]); if 0&DBUG, dbstop 670
saveas(hband2, [ FIGF  'png'], 'png') ; end; 
 end;  if 10, if  ~exist('xbf2', 'var'), xbf2= xlf2; end;   
 if  ~exist('xbf1', 'var'), xbf1= xlf1; end; banot.time = ['Wander Corrected' ]; 
stringw=[ Afilemits '  Channel 1 ', lead1 num2str( ACAS)];
windowSize = 5; CL= 200*360/1000 ; CL60=600*360/1000 ;
if ~strcmp(comp,'PCWIN64'), BLSIZE=  1000;  else BLSIZE= 10 ; end;
f21= fix ( L/BLSIZE ) ;   f2i = cast( f21, 'int64' ); f22= ceil(L/BLSIZE) ;
if ~isequal( B ,'BL'), L1= 1080; f22=1080; else L1=NS; f22= ceil(L/BLSIZE); 
end;  filsig1 = filter( ones(1,windowSize)/windowSize ,1, xbf1(1 :  L1) );    
 fprintf(' 678  step 1, 3, 6 Wander removed  ECG step 3 \n' );  
fmp1 = medfilt1(filsig1 , CL, f22  );  % plot(fmp1(1 : 1+1000-1) ,  'k'); 
fmt1  = medfilt1(fmp1 , CL60,  f22 ); %  plot(fmt1(1:1000), 'g'); hold on; 
if 0, fmp1 = medfilt1(filsig1 , 200, f22  );  % plot(fmp1(1 : 1+1000-1) ,  'k'); 
fmt1  = medfilt1(fmp1 , 700,  f22 ); %  plot(fmt1(1:1000), 'g'); hold on; 
end; if  1&&isequal( B,'BL'),   xbl1(1 :  L1 )  = filsig1 - fmt1 ;  
else   xbl1(1 : L1) = filsig1 - fmt1;  xbl1(L1+1 :  NS) = xbf1(L1+1: NS) ;   
end; end;  if 1, hwan= figure('Name', [ Afilemits '690  hwan  ' stringw ] , ...
'Position',[ scr(1)+20 screen(4)/2-90  screen(3)/2-10 screen(4)/2-20]);
h = gcf ;  set(h,'WindowStyle','docked') ; 
set( gca,'Units', 'normalized',  'FontUnits', 'points', 'FontWeight','normal',  ...
 'FontSize', fonts16, 'FontName','Times'   ) ;    toc 
 %title( ' \color{red} Red: original ECG, Blue: Baseline wander corrected ');
if 0,  plot( xbf1(1:DN) ,   ':*r'  , 'LineWidth', 2  ); hold on;  end;   
if 0, ct = camtarget; camtarget( [ct(1)+0.001*ct(1) ct(2)+0.001 ct(3)]);
ax2 = copyobj(gca,gcf);  set(ax2,'XAxisLocation','top',...
'XTickLabel','','YAxisLocation','right', 'YTickLabel','','Color','none' );
end; plot(  xbf1( 1:DN ) ,   '-r ' , 'LineWidth', 4 , 'MarkerSize', 2 ); 
set( gca,  'Units', 'normalized', 'FontUnits', 'points', 'FontWeight','normal',  ...
 'FontSize', fonts16 , 'FontName', 'Times'   ) ;  
hold on;   plot(  xbl1( 1 : DN ) , '-b' , 'LineWidth', 4 , 'MarkerSize',2  ); 
line( [ 0 , DN ] , [ 0,0 ] , 'Marker','.','Color', 'g', 'LineWidth', 2 );     
% xlim([1  DN]) ; ylim([ mind1   maxd1+0.0  ]) ;   
mind1 = min(xbf1(start:DN)) ;  maxd1 = max(xbl1(start:DN))+0.1;
if Afilemit~=112,  CELL3y = [  mind1 0 maxd1 ] ;
else CELL3y = [mind1   maxd1 ]; end;  ylim([ mind1  maxd1]) ;  
set( gca,'XTick', 0:1080/3:1086); set( gca,'XTickLabel', { 0:1080/3:1086}) ; 
for i =1:length(CELL3y) [ ST1adc{ i,1}, csT2]=sprintf( '%.1f', CELL3y(i)) ; end;
  xlim([1 1086]) ; set(gca,'YTick', CELL3y  ) ; set(gca, 'YTickLabel',  ST1adc); 
if 1,  title( ' Red:Original raw ECG, Blue:Baseline wander corrected '...
,'Color', 'r', 'FontSize',  fonts16 ); iqrs= { 'FontSize', 12 } ;
end; if 0,   strlead2=[ ' \color{red} Red:  original ECG' '   ' ...
 ' \color{blue} Blue: Baseline wander Corrected' ]  ;  
title( strlead2,   'FontSize', fonts16);
end;  ylabel(' milli Volts '  ,   'FontSize' , fonts16  , 'Color', 'b' ) ;  
xlabel(' Time in samples' , 'FontSize' , fonts16 , 'Color', 'b'  );   
clear fmp2 fmt2 filsig2   ;   % line( [ 0,0 ] , [1000 ,0] , 'Marker','.','Color', 'g' );  
clear fmp1 fmt1 filsig1; clear   fmp1; % plot(xbl1(1:1000 ) , '-.or' )
if 0, text(DRB, xbl1(DRB),  'R' , 'color', 'b' );
 for k=1:length(DRT)  text( Ra(k) , xbl1( Ra(k)) ,[ 'R'  Ca(k) ], 'color','r' ); end;
end; cnptoqc= [  'BaselineMain' Afilemits];clear  filsig1  fmt1 ;   
  FIGFILE= [ cnptoqc d(1:2), d(4:6), d(10:11), d(13:14), d(16:17) B ]; 
 FIGUFILE = fullfile(PATFIG, FIGFILE);  if  DBUG, 
saveas( hwan , [ FIGUFILE    Afilemits  B  'png' ], 'png'); 
 end;;end;if 10,   if ~isequal( B,'BL'), L1= 1080; f22= 1080; 
end;if isequal( B,'BL'),  L1= NS; f22=ceil(L/BLSIZE);  
end; fprintf(' 732  step 1, 3, 6 Wander removed  ECG step 3 \n' );   
filsig2= filter ( ones(1,windowSize)/windowSize ,1, xbf2(1 : L1) );
fmp2 = medfilt1(filsig2, 200 ,f22 ); 
fmt2 = medfilt1(fmp2, 700, f22);   
 if 1&&isequal( B,'BL'),   xbl2(1 :  L1 ) = filsig2 - fmt2  ;
 else xbl2(1: L1)= filsig2 -fmt2; xbl2(L1+1: NS)=xbf2(L1+1: NS);     
end;   ; clear fmt2 filsig2 fmp2 xr ;   toc
median(200, 700) 
zerrf1r= -xbf1(Ra(1))+xlf1(Ra(1)) ; zerrw1r=-xbl1(Ra(1))'+xbf1(Ra(1)); 
zerrf2r= -xbf2(Ra(1) )+xlf2(Ra(1)  );  zerrw2r = -xbl2(Ra(1) )'+xbf2(Ra(1) ); 
zerrA1r= zerrw1r+zerrf1r;  zerrA2r = zerrw2r+zerrf2r;
zerrf1= -xbf1(Ra )+xlf1(Ra  ) ; zerrw1=- -xbl1(Ra )'+xbf1(Ra  );  toc
zerrf2= -xbf2(Ra )+xlf2(Ra  );  zerrw2 = -xbl2(Ra )'+xbf2(Ra  ); 
zerr1 = zerrw1+zerrf1;  zerr2 = zerrw2+zerrf2;
zerrA1= xlf1(Ra' )- xbl1(Ra)' ; zerrA2 = xlf2(Ra') - xbl2(Ra)' ; 
if 0, xbf1(Ra )= xbf1(Ra ) + zerrf1; xbf2(Ra )= xbf2(Ra )+zerrf2;  
    xbl1(Ra ) = xbl1(Ra )'+ zerr1; xbl2(Ra ) = xbl2(Ra )'+zerr2;  
end;if 10, hwan2 = figure('Name',[ '725M' stringw ] , ...
 'Position', [1366/2 768/2-40 1366/2-10 768/2-40]);
h=gcf; set(h,'WindowStyle','docked'); 
plot( xbl2(1:DN), 'r'  , 'LineWidth', 2 ); hold on;
 plot(xlf2(1:DN ),  'b'  , 'LineWidth', 2  );   
 set( gca,  'Units', 'normalized',  'FontUnits', 'points', 'FontWeight', 'normal',  ...
 'FontSize', fonts16, 'FontName', 'Times' );  [ ARCN]=find( C == '~' );  
 if 0,  BPd1=BP1+10; , BPd2=BP2-10; ,
    [ xlf2, a1,b1]=butterbpass(xbl2, BPd1, BPd2,order , k2 ,Afilemits ) ;
 hold on;  plot(time, xbf2( time) ,  'color', 'g'   , 'LineWidth', 2  ); 
 hold on;  plot(time, xlf2( time) ,  'color', 'm'   , 'LineWidth', 2  ); 
xbl2=xlf2; clear xlf1 xlf2;
end;end; end; if   RTilde>=1,
    for i= 1 : RTilde-1 ARCNd(i)=ARCN(i+1)-ARCN(i ); end;
[ARCNdmin ARCNdI]=min(ARCNd);v1=ARCN(ARCNdI);w1=ARCN(ARCNdI+1);  
if ( Afilemit==101), hi=0.3; xb=[ v1-1,  w1+1 ] ;
U=RO(v1-1)-200;V=RO(w1+1) +300;U1= RO(v1)-300; V1= RO(w1+1)+200;  
elseif ( Afilemit==207),  hi=0.3; ROM60 =RO(v1-1)-100 ;     
U=RO(v1-2)-200;V= RO(w1+2 )+200; xb=[v1-2,v1-1,v1+1: w1-1, w1+1,w1+2,];
elseif ( Afilemit==215), ROM60 =RO(v1)-100; hi=0.3; ROM60 =RO(v1-1)-100 ;     
U=RO(v1-2)-200; V= RO(w1+2 )+200; xb=[v1-2,v1-1,v1+1: w1-1, w1+1, w1+2,];
else U=RO(v1-2)-200; V=RO(w1+2 )+200; xb=[ v1+1: w1-1]; hi=0.2;
end; end;end; if RTilde>=1,hnoise=figure('Name', [ '755 noise' Afilemits ...
 RTildes], 'Position', [ 20  40  screen(3)-120 screen(4)-140 ] ); 
if U<0, U=1; end; h = gcf ;  set(h,'WindowStyle','docked') ; 
ROM60 = RO(v1) ;   plot( U : V, xbl1( U : V) , cb , 'LineWidth', 2 ); %thesis
set( gca,  'Units', 'normalized',  'FontUnits', 'points', 'FontWeight','normal',  ...
'FontSize', fonts16, 'FontName','Times' ) ; RA3=[ ]  ; 
if BL, line( [ U , V ] , [ 0 , 0 ] , 'Marker', '.'  , 'Color', 'k'  );  
  elseif 1,  line([ U, V], [ 0,0 ] , 'Marker' , '.', 'Color', 'k' , 'LineWidth', 1);         
end; mn2= min(xbl1(U : V ))-0.2 ; mx2= max(xbl1(U : V))+0.2;  
if ( Afilemit==208 ), hi =0.1;
mn2=min(xbl1(U : V ))-0.2; mx2= max(xbl1(U : V))+0.32;  
else mn2=  min(xbl1(U : V ))-0.2 ; mx2= max(xbl1(U : V))+0.32;  
end; fyTilde= ( mn2 :(mx2- mn2 )/2: mx2 ) ;  set(gca, 'YTick', fyTilde ) ; 
text( ROM60, xbl1(RO(v1 ))+0.4 ,...
[ '\downarrow' int2str( v1 ) C(v1) int2str(RO(v1)) ] ,'color', cr ,'FontSize' ,16 ) ; 
if 0,text( RO(w1), xbl1(RO( w1))-1*hi , ...
[ '\uparrow' int2str(w1 ) C(w1) int2str(RO( w1)) ], 'color', cr , 'FontSize' ,16 );
end; if 10, text( RO(w1 ), xbl1( RO(w1 ))+1*hi ,...
 ['\downarrow' int2str(w1 ) C(w1) int2str(RO( w1))] , 'color', cr, 'FontSize',16 ); 
end; if ( Afilemit~=101),    for v2=xb   
text(RO(v2), xbl1(RO( v2)), [ int2str(v2) C(v2) int2str(RO( v2))], ...
 'color', cb, 'FontSize' ,16 );
end; end;  if ( Afilemit==101),  text(RO(v1-1), xbl1(RO( v1-1 ))+0.25 , ...
[  int2str(v1-1) C(v1-1) int2str(RO( v1-1) )  ], 'color', cb , 'FontSize' ,16);
text(RO(w1+1),xbl1(RO(w1+1))+1*hi,[int2str(w1+1) C(w1+1) int2str(RO(w1+1))],...
 'color', 'b'  , 'FontSize' ,16  );
end; if 0, text(RO(w1+2), xbl1(RO( w1+2)), ...
 [ int2str(w1+2) C(w1+2) int2str(RO(w1+2))], 'color', 'b'  , 'FontSize' ,16);
 end;  for i = 1 : length( fyTilde ) 
 [ STfyxbf1{ i,1} , errmsg] = sprintf( '%.2f', fyTilde(i) ) ; 
end;  set( gca,  'YTickLabel',  STfyxbf1 ) ;    xlim([U  V]) ; 
set( gca, 'XTick', U: fix((V-U)/2):V ); set(gca, 'XTickLabel', U:fix((V-U)/2):V) ;    
title( [ ' Noise annotations in Class label list, Record  ' Afilemits ], ...
 'color', 'r' ,  'FontSize' ,16 ) ; ylim([ mn2  mx2  ]) ;  
 ylabel(   stringcell3{1}   , 'Color', 'b' , 'FontSize', fonts16 ); 
  ylabel('Amplitude (mV) ', 'FontSize', 18, 'Color', 'b' );   
 xlabel( 'Time in samples, 360 samples  per second', ...
 'Color', 'b' , 'FontSize', fonts16 ) ;   cnptoqc = [  'hnoise' Afilemits ] ;  
 FIGFILEn = [ cnptoqc d(1:2), d(4:6), d(10:11), d(13:14), d(16:17) B ];
FIGUFILEn = fullfile(PATFIG, FIGFILEn);if  DBUG, ,
 saveas(  hnoise , [ FIGUFILEn Afilemits B 'png'], 'png');    
end;end; [ AFL ]=find( C == '{' );  [AFR]=find( C== '}' ) ; 
RTild=(length(AFL)') ;  RTilds=int2str(RTild )
 if RTild>=1, h2AFL7= figure('Name', ['800 FLutter' Afilemits  ...
RTilds], 'Position', [ 20 40  screen(3)-120 screen(4)/1-140] ) ;  
h = gcf ;  set(h,'WindowStyle','docked') ; 
%h2AFL7g =get(gcf,'CurrentAxes'); set( h2AFL7,'CurrentAxes',h2AFL7g);
if RTild >=1, for i= 1: RTild-1  AFLUd(i) = AFR(i)-AFL( i );  end;
[ AFLUdmin  AFLUdI] =min( AFLUd );   
v1=AFL(AFLUdI);   w1=AFR(AFLUdI ); vf= v1; wf= w1; 
end;  U= RO(v1-1)-100; V = RO(wf+2 )+400; V1 = V; if U<0, U=1; end;
 mn2=min(xbl1(U: V ))-0.32 ; mx2= max(xbl1(U :V))+0.32;  
 plot( U : V, xbl1( U : V) ,cb , 'LineWidth', 2   ); xlim([ U  V ]) ; 
if BL, line([ U ,V ], [ 0 , 0 ] , 'Marker' , '.', 'Color', 'b' , 'LineWidth', 2);
 elseif 1,  line([ U, V], [ 0,0 ] , 'Marker' , '.', 'Color', 'k' , 'LineWidth', 1);          
end; set( gca,  'Units', 'normalized',  'FontUnits', 'points', 'FontWeight', 'normal',  ...
 'FontSize', fonts16 , 'FontName', 'Times'   ) ;   v2rf=[ v1+1: w1-1  ]
ROM=[ C(v1)  int2str(RO( v1)) '\uparrow'  int2str(RO(v1-2)) 'f' int2str(RO(v1)) ] ;
 ylim([ mn2  mx2 ]); fyTilde= ( mn2 :(mx2- mn2 )/2: mx2); x_a = [ '\downarrow' ] ;
 if 0, for v2= v1+1: w1-1 C(wf+5) 
text( RO( v2 ), xd1(RO( v2 )), [ int2str(v2)], 'color', 'b', 'FontSize' ,16 );
end; end; if 10, if 0, text(RO(v1-2), xbl1(RO( v1-2))+0.35*(hi) , ...
 [  int2str(v1-2) C(v1-2) ], 'color', cb , 'FontSize' ,fonts  );
end; RA3=[ int2str(RO( v1-1) ) int2str(RO(v1-1 ))  ]; 
 text(RO(v1-1), xbl1(RO( v1-1 ))-0.11*(hi) , ...
 [   int2str(v1-1) C(v1-1) int2str(RO(v1-1))  ], 'color', cb , 'FontSize' ,16);
text( RO(v1)-23, xbl1(RO(v1)-3)+0.24    ,...
[ int2str(v1) 'FL' int2str(RO(v1  ))   ] , 'color', cr  , 'FontSize' , 16 ) ; 
if 0,text( RO(v1+1) , xbl1(RO(v1+1 ) )+0.2*hi ,  ...
[ int2str(v1+1) C(v1+1) ], 'color', cr  , 'FontSize' , 16 ); 
 text(RO(v1+2), xbl1(RO( v1+2))-0.6   ,    ...
[ int2str(v1+2) C(v1+2)  ] , 'color', cb , 'FontSize',16);
 text( RO( v1+3 ), xbl1(RO( v1+3 ))-1.2*(hi) , ...
[ int2str( v1+3) C( v1+3)], 'color', cb , 'FontSize' ,16 );
 text( RO( v1+4) , xbl1(RO( v1+4 ))-2*(hi) , ...
[ int2str( v1+4) C( v1+4)] , 'color', cr , 'FontSize' ,16 );
end; if 0,   text( RO(v1+2)+25, xbl1(RO( v1+2)+5)+ 0.3 , ...
[ int2str(v1+2) C(v1+2)  ] , 'color', cr , 'FontSize',16  ) ;
text( RO( v1+3 ), xbl1(RO( v1+3 ))+0.7 , ...
[ int2str( v1+3) C( v1+3)], 'color', cr , 'FontSize' ,16 ) ;
 text( RO( v1+4)+6 , xbl1(RO( v1+4 )+6 )+1.470  , ...
[ int2str( v1+4) C( v1+4) ] , 'color', cr , 'FontSize' ,16 );
 text( RO( v1+5 )+80 , xbl1(RO( v1+5 ))+1.262 , ...
[ int2str( v1+5) C( v1+5) ], 'color', cr , 'FontSize' ,16 ) ;
text( RO( v1+6)+40 , xbl1(RO( v1+6 )+1)+0.5 , ...
[ int2str( v1+6) C( v1+6)   ], 'color', cr , 'FontSize' ,16 )  ;
text( RO( v1+7)+40 , xbl1(RO( v1+7 ))+0.81 , ...
[ int2str( v1+7) C( v1+7) ], 'color', cr , 'FontSize' ,16 ) ;
text( RO( v1+8 ) , xbl1(RO( v1+8 ) )+0.87 , ...
[ int2str( v1+8) C( v1+8)  ] , 'color', cr , 'FontSize' ,16);
ROM=[int2str(RO(v1+8))];
text( RO(wf-5)+45, xbl1(RO( wf-5)+45)+1.03 , ...
 [ int2str(wf-5) C(wf-5) ] , 'color' , cr, 'FontSize',16  )  ;
text( RO(wf-4), xbl1( RO( wf-4))+0.92   ,...
 [ int2str(wf-4) C(wf-4) ] , 'color' , cr , 'FontSize',16  ) ;
text(RO(w1-3)+18, xbl1(RO( w1-3)+18 )+1.12 , [ int2str(w1-3) C(w1-3)] , ...
'color' , cr , 'FontSize',16 ) ;   AMP_S=  [ int2str(wf+2) C(wf+2)];
 end; if 0, text( RO(wf-2)-5, xbl1(RO( wf-2) )+0.1  ,...
[ int2str(wf-2) C(wf-2) ] , 'color' , cr , 'FontSize',16 ) ;  
text(RO(wf-1), xbl1(RO( wf-1 ))+ 0.2*(hi) , ...
[ int2str(wf-1) C(wf-1)  ], 'color', cr , 'FontSize' ,16  );
end;text( RO( w1) , xbl1(RO( w1 ))+ 0.12 , ...
[ int2str(w1) 'FL' int2str(RO(w1 ))], 'color', cr , 'FontSize' ,16 ) ;   
if 0, text(RO(w1+1), xbl1(RO( w1+1)),...
        [ int2str(w1+1) C(w1+1) int2str(RO(w1))] ,...
'color',cr, 'FontSize',16 ); 
end;Annconpto=[ int2str(wf+2) C(wf+2) int2str(RO(wf+2))]
 text(RO(wf+2), xbl1(RO( wf+2))+0.12*(hi) ,   ...
 [ int2str(wf+2) C(wf+2)  int2str(RO(wf+2)) ], 'color', 'b'  , 'FontSize',16  );
end; set( gca, 'YTick', fyTilde ) ; 
for i= 1: length( fyTilde ) [ STfyxbf1{ i,1}, esg ]= sprintf('%.2f', fyTilde(i)); end; 
 set( gca,  'YTickLabel',  STfyxbf1 ) ; xlim([ U  V ]) ;  ylim([ mn2  mx2  ]) ;
set( gca, 'XTick', U: fix((V-U)/2) :V ); set( gca, 'XTickLabel', U:fix((V-U)/2):V);    
 title( [ 'Flutter  annotations in Class label list, Record  ' Afilemits ] , ...
 'color' , 'r' , 'FontSize' ,16 );clear xdl1;clear xd1; 
ylabel( 'Amplitude, mV' ,  'FontSize', fonts16  , 'Color', 'b' );
xlabel('Time in samples, 360 samples  per second', ...
 'Color', 'b' ,'FontSize', fonts16 ); cnptoqc = [  'h2AFL7' Afilemits ] ;  
 FIGFILE= [ cnptoqc d(1:2), d(4:6), d(10:11), d(13:14), d(16:17) B ];
  FIGUFILf = fullfile(PATFIG, FIGFILE); if DBUG,  dbstop 886; 
saveas( h2AFL7 , [  FIGUFILf Afilemits  B  'png'] , 'png') ;    
 end; end;  if 10, [ AFL ]=find( C == '{' );  [AFR]=find( C== '}' ); 
AFLUTL=(length(AFL)') ;  AFLUTLs=int2str(AFLUTL )
 if AFLUTL>=1, h2AFL=figure('Name', ['880 FLutter' Afilemits AFLUTLs], ...
'Position', [ 20 40  screen(3)-120 screen(4)/1-140 ] ) ; 
h = gcf ;  set(h,'WindowStyle','docked') ; 
%h2AFLg= get( gcf , 'CurrentAxes' ); set( h2AFL, 'CurrentAxes', h2AFLg) ;
if AFLUTL>=1, for i=1: AFLUTL-1  AFLUd(i) = AFR(i)-AFL( i);  end;
[AFLUdmin  AFLUdI] = min(AFLUd );   
v1=AFL(AFLUdI);  w1=AFR(AFLUdI ); vf= v1-1; wf= w1+2; 
end;  U= RO(v1-1)-100; V = RO(wf+0 )+400; if U<0, U=1; end;
 plot( U : V, xbl1( U : V) ,cb , 'LineWidth', 2  );   ROM=[int2str(RO(v1+8))];
 if 10,xr=-0.01; line([ U,V ], [xr , xr] , 'Marker' , '.', 'Color', 'b' , 'LineWidth', 2);
 end; set( gca,'Units', 'normalized', 'FontUnits', 'points','FontWeight', 'normal', ...
 'FontSize', fonts16 , 'FontName', 'Times'   ) ;   v2rf=[ v1+1: w1-1  ]
 mn2=min(xbl1(U: V ))-0.32 ; mx2= max(xbl1(U :V))+0.32;  
ROM=[ C(v1)  int2str(RO(v1))  '\uparrow'  int2str(RO(v1-2)) 'f' int2str(RO(v1))];
 ylim([ mn2  mx2 ]); fyTilde= ( mn2 :(mx2- mn2 )/2: mx2); x_a=[ '\downarrow'] ;
v2v=[ v1-2  v1-1 wf ];v2vm=[    ] ;v2=[ v1-1 wf  ]
 if 10,   [ v2m ve2mi] =  find( RO(v1-1 )== Ra);  
  text( Ra( v2m ), xbl1(Ra( v2m ))-0.1, ...
    [ int2str(v2m )  Ca(v2m ) int2str(Ra(v2m)) ], 'color', 'b', 'FontSize' ,16 );
 [ wfm wfmi] =  find( RO(wf )== Ra);  
  text( Ra( wfm ), xbl1(Ra( wfm ))+0.1 , ...
    [ int2str(wfm )  Ca(wfm ) int2str(Ra(wfm)) ], 'color', 'b', 'FontSize' ,16 );
 end; if 1, text( RO(v1)-0, xbl1(RO(v1)-0)+0.20 ,...
[ 'FL'  C(v1) int2str(RO(v1)) ] , 'color', cr  , 'FontSize' , 15 ) ; 
text(RO(w1),xbl1(RO( w1))+0.12,...
    ['FL' C(w1) int2str(RO(w1))],'color', cr,'FontSize',15);
end; if 0,  text( RO(w1-2)-5, xbl1(RO( w1-2) )+0.1  ,...
[ int2str(w1-2) C(w1-2) int2str(RO(w1-2))] , 'color' , cr , 'FontSize',16 ) ;  
text(RO(w1-1), xbl1(RO( w1-1 ))+ 0.2*(hi) , ...
[ int2str(w1-1) C(w1-1) ], 'color', cr , 'FontSize' ,16  ); int2str(RO(w1+2))
text(RO(w1+1),xbl1(RO( w1+1)), [ int2str(w1+1) C(w1+1)] ,'color',cr,...
'FontSize',16); Annconpto=[ int2str(w1+2) C(w1+2) int2str(RO(w1+2))]
end; set( gca, 'YTick', fyTilde ) ; 
for i= 1: length( fyTilde ) [ STfyxbf1{ i,1} , err]= sprintf('%.2f',  fyTilde(i)) ; end;
  set( gca,  'YTickLabel',  STfyxbf1 ) ; xlim([ U  V ]); 
set( gca, 'XTick', U: fix((V-U)/2) : V ); set( gca, 'XTickLabel', U:fix((V-U)/2):V);    
 title( [ 'Flutter  annotations in Class label list, Record  ' Afilemits  ] ,  ...
 'color' , 'r' , 'FontSize' ,16 );ylim([ mn2  mx2  ]) ; 
 ylabel( 'Amplitude, mV' ,  'FontSize', fonts16  , 'Color', 'b' );
xlabel('Time in samples, 360 samples  per second', ...
 'Color', 'b' , 'FontSize', fonts16 );   cnptoqc = [  'h2AFL' Afilemits ] ;  
 FIGFILE = [ cnptoqc d(1:2), d(4:6), d(10:11), d(13:14), d(16:17) B ];
  FIGUFILE = fullfile(PATFIG, FIGFILE); if  DBUG,  dbstop 932; 
saveas( h2AFL  , [  FIGUFILE Afilemits  B  'png' ] , 'png' ) ;    
end; end; end; if 0,  hDiff = figure('Name', [ '930 Diff' Afilemits ]  , ...
 'Position',[ 20  screen(4)/2-40  screen(3)/2-40 screen(4)/2-40]) ; 
strlead1= [ ' Diff ' (Afilemits) ' Channel 1: ', lead1 ]; subplot(2,1,1);V1= DN ; 
 min1180 = min(xbl1(start: DN)) ; max1525= max(xbl1(start: DN))  ;
set( gca,  'Units', 'normalized', 'FontUnits', 'points',  ...  
'FontWeight', 'normal', 'FontSize', fonts18, 'FontName', 'Times' ); 
 U= start; V= DN ;   fyxbf1 =[ min1180 0   max1525 ] ;ST3adc
  ylabel( 'Amplitude (mV)'  ,  'FontSize', 17  , 'Color' , 'b'  );
 if  exist('xbf1', 'var'), plot(xbf1(1:DN)  ,cb , 'LineWidth', 2 );  hold on; end;    
 title2 =[ 'Q R S Detection Record  ', Afilemits ];  
 title( title2 , 'color', 'r' ,'FontSize', fonts18 ); set( gca,'XTick', 0:1080/3:1086); 
set( gca,'XTickLabel', { 0:1080/3:1086}) ; xlim([ 1  DN ]) ; 
 line( [ 0,DISP] , [ 0,0 ] , 'Marker' , '.'  , 'Color', 'r');  set( gca, 'YTick', fyxbf1) ; 
for i= 1: length( fyxbf1 ) [ STfyxbf1{ i,1} , errg] = sprintf('%.2f', fyxbf1(i)) ; end;
  set( gca,  'YTickLabel',  STfyxbf1 ) ;  ylim([min1180  max1525  ] ) ;
  subplot(2,1,2); set(gca,  'Units', 'normalized',    'FontUnits', 'points', ...
  'FontWeight', 'normal',  'FontSize',  14, 'FontName','Times' );  
 if  exist( 'xdiff', 'var') plot(xdiff(1: lxd) , 'r'  , 'LineWidth', 2 ) ;  hold on;
end; minxdiff  =min(xdiff( 1: lxd) ) ; maxxdiff = max(xdiff(1: lxd)); 
fyxdiff =[ minxdiff  0  maxxdiff ] ;  set( gca,  'YTick', fyxdiff  ) ; 
for i= 1: length( fyxdiff )  [ STfyxdiff{ i,1} ,erg] =sprintf(' %.2f', fyxdiff(i)); end; 
 set( gca,  'YTickLabel',  STfyxdiff ) ;  ylim([ minxdiff maxxdiff  ]) ;   
line( [0, DISP] , [ 0,0 ] , 'Marker' ,  '.' , 'Color', 'r');  
text( DRB-1, xdiff(DRB-2), 'R',  'color', 'c'  ,  'FontSize', fonts18  ); 
titleqrs =[ ' Difference operation   Record  ', Afilemits ]; 
 title( titleqrs , 'color', 'r' ,  'FontSize', fonts18 ) ;  xlim([1  DN]) ; 
xlabel('Time in samples, 360 samples per second','Color', 'b','FontSize',fonts16); 
 ylabel( 'Amplitude' , 'Color', 'b' ,'FontSize', fonts16);cnptoqc = ['Diff' Afilemits] ;  
 ylabel('Amplitude, mV'  ,   'FontSize', fonts16  , 'Color', 'b'  ) ;
set( gca, 'XTick' , 0:1080/3:1086 ); set( gca, 'XTickLabel', { 0:1080/3:1086});
FIGFILE = [ cnptoqc d(1:2), d(4:6), d(10:11), d(13:14), d(16:17) B ];
FIGUFILE = fullfile(PATFIG, FIGFILE )  
 end; if 10, mvP = 0.48*10^-3;
t1= 2* mvP; mvN = -0.47*10^-3; t2= 2* mvN;
for i= 1 : 1080     xdiff(i)=xbl1(i +1)-xbl1(i );  end; lxd = length(xdiff); 
for i= 1: length(xdiff)  if ((xdiff(i) >= t1)|(xdiff(i) <= t2)), xdfc(i) = xdiff(i);
elseif  ( (0 <= xdiff(i)< t1) || (t2<xdiff(i)<= 0) ), xdfc(i)=0;else xdfc(i)=0;
   end;end; q=0;  for i=1 : length(xdiff)   xdfcp(i)= 0;  xdfcn(i)= 0; end;
for i=1:  length(xdiff)  if ( xdfc(i) > 0),   xdfcp(i)=xdfc(i);
elseif(xdfc(i) < 0), xdfcn(i)=xdfc(i) ;   
 else   xdfcp(i)= 0;  xdfcn(i)= 0; q=q+1;  pq(q)= i ;    
 end; end; end; if 10, strlead2 =[ ' 970 xdiff ' (Afilemits) lead2 ]; 
hrxdiff = figure( 'Name' , strlead2,  'Position', ...
[20 40 screen(3)/2-40 screen(4)/1-140]);h =gcf; set(h,'WindowStyle','docked'); 
set(gca,  'Units', 'normalized',  ...  
 'FontUnits' ,  'points',  'FontWeight', 'normal', ...
 'FontSize', fonts16 , 'FontName', 'Times' );  
 if (0),  minxdiff  =min(xdfc( 1: lxd) ) ; maxxdiff   =max(xdfc(1: lxd)); 
 axis([start, DN  ,min(xbl1(1: DN) )-0.1,max(xbl1(1: DN))+0.1]); 
line([ 0,1080 ], [0,0] , 'Marker' , '.', 'Color', 'b' , 'LineWidth', 1);
 end; if  exist('xdfc', 'var'), subplot(3,1,1);
 plot(xdfc( 1: DN ), 'g', 'LineWidth', 2 );  hold on; 
minxdfc = min(xdfc(1: DN ) )-0.01;  maxxdfc=max(xdfc(1: DN ) ) ;  
fxdfc=  [minxdfc   0  maxxdfc ];   set( gca,  'YTick' , fxdfc  ) ; 
for i= 1: length( fxdfc ) [ STxdfc{ i,1} , erg] = sprintf('%.2f', fxdfc(i) ); end;  
 set( gca,  'YTickLabel',  STxdfc ) ;  ylim([ minxdfc maxxdfc  ]) ;  
set( gca,  'Units', 'normalized', 'FontUnits','points' , ...
 'FontWeight', 'normal',  'FontSize', fonts18 , 'FontName','Times') ;  
set( gca,'XTick', 0:1080/3:1086); set( gca,'XTickLabel', { 0:1080/3:1086}); 
title( [' Difference signal  Record  ', Afilemits] ,'color', 'r' ,  'FontSize' ,18 ); 
ylabel('Amplitude, mV'  ,   'FontSize', fonts16  , 'Color', 'b' );   xlim([1  DN]) ;
titleqrs=[ 'Difference operation   Record  ', Afilemits ];
title( titleqrs , 'color', 'r' , 'FontSize', fonts18 ) ; 
end; if  exist( 'xdfc', 'var'),   subplot(3,1,2) ;  
plot(xdfcn(1: DN  ) ,'c', 'LineWidth', 2 );    hold on; 
plot(xdfcp(1: DN  ),  'm', 'LineWidth', 2 );  hold on; 
if 10,  text(  DRB-0 , xdfcn(DRB-0),  'R', 'color', 'r' , 'FontSize', 16 );   
else  for k=1:length(DRT) text(Ra(k) ,xbl1(Ra(k)), [ 'R' Ca(k)], 'color','r' ); end;  
end;  set(gca,  'Units', 'normalized',  'FontUnits', 'points' , ...
 'FontWeight', 'normal',  'FontSize', fonts18 , 'FontName', 'Times'   ) ; 
 set( gca,  'XTick', 0:1080/3:1086 );set( gca,'XTickLabel', { 0:1080/3:1086}); 
 minxdfcn  = min(xdfcn(1: DN) )-0.01;  maxxdfcp   =max(xdfcp(1: DN) ) ;  
xlim([1  DN]);  fyxdfc =  [ minxdfcn  0  maxxdfcp ] ; set( gca,  'YTick', fyxdfc); 
for i= 1: length( fyxdfc ) [ STfyxdfc{ i,1} , ers] = sprintf('%.2f',  fyxdfc(i) ); end;
  set( gca,  'YTickLabel',  STfyxdfc );  ylim([ minxdfcn   maxxdfcp  ]) ;  
 title( [' Seperated Positive and Negative signals '] ,'color', 'r','FontSize' ,18);   
ylabel( 'Amplitude, mV'  ,   'FontSize', fonts16  , 'Color' , 'b' );
 end; if 1,  subplot( 3,1,3);      
 if  exist('xdfc', 'var'), plot(xbl1(1: DN  ) ,'r', 'LineWidth', 2); hold on; 
set(gca,  'Units', 'normalized','FontUnits','points' , ...
 'FontWeight', 'normal',  'FontSize', fonts18 , 'FontName','Times') ; 
  set( gca,'XTick', 0:1080/3:1086) ; set( gca,'XTickLabel', { 0:1080/3:1086}) ; 
  minxf =min(xbl1( 1: lxd) )-0.1 ; maxf=max(xbl1(1: lxd)); 
if 0<maxf &&0>minxf, fyxf =[minxf 0 maxf]; elseif minxf<maxf, fyxf=[minxf maxf];
end; set( gca,  'YTick', fyxf  ) ; 
for i= 1: length( fyxf ) [ STfyxf{ i,1} , errmsg] = sprintf('%.2f',  fyxf(i) );  end; 
 set( gca,  'YTickLabel',  STfyxf );  ylim( [ minxf maxf  ]) ; xlim([1  DN]) ; 
if Afilemit==101,text(DRB , xbl1(DRB)+0.12, 'R', 'color', 'm' , 'FontSize', 16);  
else  text(DRB , xbl1(DRB )+0.12,  'R', 'color', 'm' , 'FontSize', 16 );  
end; line( [ 0, DISP] , [ 0 ,0 ] , 'Marker' ,   '.'  , 'Color', 'r' );  
 ylabel('Amplitude, mV' , 'FontSize', fonts16  , 'Color', 'b' );
xlabel('Time in samples, 360 samples  per second',  ...
 'Color', 'b' , 'FontSize', fonts16 ) ;  
title([' R positions '] ,'color', 'r' ,'FontSize',18); cnptoqc= [ 'hrxdiff' Afilemits]; 
end; end;  xlim([1 DN]) ;dates
FIGFILE= [ cnptoqc B dates   Afilemits '.png' ] ;
 FIGUFILdf = fullfile( PATFIG, FIGFILE );if DBUG, 
saveas(hrxdiff , [ FIGUFILdf ] );  
end; end;  if 1, RL= length(Ra); diff1 = setdiff( RO, RA) ; 
 clear ecg_d Range; clear  DD  rid icr  AR Rtime RBB Cbbr CRB CORB ; 
clear  a b a1 b1  fm1 fm2 filsig   fy ecg1; clear ANNCONP   ROCNPTO ;
fprintf(' 1020  Bandpass Freq: cof1=%f  cof2=%f  \n', cof1, cof2 );
clear xd ecg ecg2 ANNRO   xhf xnf    clear xd1; clear(['ecg' Afilemits ]) ; 
 % [left bottom width height] if  exist('xbl1', 'var'),xr = xbl1; end; 
 if ( Afilemit ==111) , TPRANGE=10; else TPRANGE = 20;  end;
 RRATSUM =0;  RR=[]; r=0;nl = 1; MVqq=[];  mvp =[];  
 title2 = [ ' Q R S detection Record  ', Afilemits ];  
 if ~exist('xbf1', 'var')  &&  exist('xd1', 'var'), xbf1= xd1; end; 
 clear xlf1 xlf2 xd2;   clear xd1 xbf1 xbf2;   format short g;; 
 end;  end; for  i = 1  :  RL    k = i;
   [ If  ] =  find ( RO == Ra(i) ) ; 
 if   (Afilemit ==208 ), if  Ra(i) == 853 ,  Ra(i)   = 858;  
 elseif  Ra(i) == 1579 ,  Ra(i)   = 1584; elseif  Ra(i) == 3411,Ra(i)= 3418;
 elseif  Ra(i)  == 8317,   Ra(i)   = 8322;
end;  end; if (Afilemit == 213 ),  if  Ra(i)== 49998, Ra(i) = 50018;
 elseif  Ra(i) == 60165, Ra(i)=60185; elseif  Ra(i) == 71534, Ra(i)= 71554;
  elseif  Ra(i  )   == 117029 ,    Ra(i  )   = 117050;
elseif  Ra(i)==117520, Ra(i)=117543; elseif  Ra(i)==120663, Ra(i)=120683;
          elseif  Ra(i  )   == 174268 ,    Ra(i  )   = 174289 ;
          elseif  Ra(i  )   == 177029 ,    Ra(i  )   = 177050;
          elseif  Ra(i  )   == 180193 ,    Ra(i  )   = 180215;
 elseif  Ra(i)==197911,Ra(i)= 197933;elseif Ra(i)== 203836,Ra(i)=203858 ;
          elseif  Ra(i  )   == 206003 ,    Ra(i  )   = 206024 ;
          elseif  Ra(i  )   == 225777 ,    Ra(i  )   = 225798 ;
          elseif  Ra(i  )   == 279271 ,    Ra(i  )   = 279293 ;
elseif  Ra(i  )   == 282458  ,    Ra(i  )   = 282479 ;
elseif  Ra(i)==286436, Ra(i) =286457; elseif Ra(i)==291587,Ra(i)=291607;
elseif  Ra(i  ) == 377477 ,    Ra(i  )   = 377499 ;
          elseif  Ra(i  )   == 381497 ,    Ra(i  )   = 381519 ;
          elseif  Ra(i  )   == 384697 ,    Ra(i  )   = 384719 ;
          elseif  Ra(i  )   == 402968 ,    Ra(i  )   = 402996 ;
elseif Ra(i)==405364, Ra(i)= 405394; elseif  Ra(i)==428716, Ra(i)=428744;
elseif  Ra(i  )== 486464 ,Ra(i )   = 486493 ;
  elseif  Ra(i  )   == 495795   Ra(i  )   = 495823 ;
  elseif  Ra(i  )   == 500262 ,    Ra(i  )   = 500291 ;
 elseif  Ra(i  )   == 514057 ,    Ra(i  )   = 514086 ;
elseif  Ra(i  )   == 592570  ,    Ra(i  )   = 592600;
elseif  Ra(i  )   == 622969 ,    Ra(i  )   = 622999 ;
elseif  Ra(i  )   == 643430 ,    Ra(i  )   = 643460 ;
end;  end;  if  (Afilemit ==233),
   if   (i == 3) , Ra(i )= 496 ; elseif  i   == 8,    Ra(i)   = 1579 ;
   elseif  i   == 11,    Ra(i )   = 2177 ; 
   elseif  i   == 15 ,    Ra(i)= 3033 ;   elseif   i   == 22 ,    Ra(i  )   = 4523 ;
 elseif   i   == 24 ,    Ra(i  )   = 4969;   elseif  i   == 28 ,    Ra(i)   = 5770;
 elseif     i   == 39 ,    Ra(i  )   = 8077; elseif   i   == 43 ,    Ra(i )   = 8922;
 elseif     i   == 49 ,    Ra(i)   = 10228;  elseif     i   == 52 ,Ra(i  )   = 10831;
 elseif  i   == 60 ,    Ra(i  )   = 12468;  elseif i   == 69 ,    Ra(i)   = 14336;
  elseif     i == 98 ,Ra(i  )   = 20306;    elseif   i  == 171 ,   Ra(i)= 35165;
%elseif  Ra(i  )   == 511 ,    Ra(i  )   = 500 ;
elseif     i   == 367 ,    Ra(i  )   = 75832 ;
 end;  if  Ra(i) == 1107 ,    Ra(i  )   = 1098 ;
elseif Ra(i) == 19443,Ra(i)= 19461 ;  elseif  Ra(i) == 22304 ,Ra(i)= 22320 ;
elseif   Ra(i)== 31909, Ra(i) = 31927; elseif  Ra(i) ==33120, Ra(i) = 33138 ;
 elseif  Ra(i)== 33937 ,    Ra(i)   = 33955 ;
          elseif  Ra(i  )   == 36740 ,    Ra(i  )   = 36757 ;
          elseif  Ra(i  )   == 39956 ,    Ra(i  )   = 39974 ;
          elseif  Ra( i  )   == 45107 ,    Ra(i  )   = 45125 ;
 elseif Ra(i) == 50721,Ra(i)= 50742 ; elseif  Ra( i) == 68780,Ra(i) = 68798;
elseif  Ra( i)== 66932 ,Ra(i)= 66950 ; elseif  Ra(i) ==178048,Ra(i)=178070;
elseif  Ra(i  )   == 211851,    Ra(i  )   = 211875 ;
elseif  Ra(i  )   == 397256,    Ra(i  )   = 397284 ;
 end;   end;   RO( If ) =  Ra( i )  ; RR(k)= Ra(i); 
  if (i -1<=0), RRdur(i) = Ra(i+1)- Ra(i); else RRdur(i) = Ra(i) - Ra(i-1) ;  
  end;  RRATSUM = RRATSUM+RRdur(i) ; 
  si1b = Ra(i)-20;    if ( si1b <= 0 ) si1b=1; end;
 if ( xbl1( Ra( i ))> 0),  [q1valmin(k),pq1(k)] =min(xbl1(si1b:Ra(i)-1));
 else    [q1valmin(k),pq1(k)] = max(xbl1(si1b:Ra(i)-1));
end;   Q1(k)= si1b+ pq1(k); si2b= Ra(i)-40; % 40 *200/100
if si2b <= 0, si2b=1; end;
if (xbl1(Ra(i)) >  0),   [q2valmin(k),pq2(k)]=min(xbl1(si2b:Ra(i)));
else     [q2valmin(k),pq2(k)] = max(xbl1(si2b:Ra(i)-1));%20/360 = 0.055556
end;  Q2(k)= si2b+ pq2(k); si1e =Ra(i) + 20; 
si1eold = si1e ; if (si1e> NS)  si1e=NS-1; end;   
 if (xbl1(Ra(i)) > 0 ) ,    [s1valmin(k),ps1(k)]=min(xbl1(Ra(i):si1e));
else    [s1valmin(k),ps1(k)] = max(xbl1(Ra(i):si1e));%40/360 = 0.111
  end;   S1(k)= Ra(i)+ ps1(k);     si2e(k) = Ra(i) + 40;
 if ( si2e(k) > NS) si2e(k)=NS-1; end;
 if (xbl1(Ra(i)) > 0 ) ,    [s2valmin(k), ps2(k)]=min(xbl1(Ra(i):si2e(k)));
%   [valmaxTP(i),TPI2(i)]=max(xbl1(Ra(i):si2e(i)));
else    [s2valmin(k),ps2(k)]=max(xbl1(Ra(i):si2e(k)));
   % [valmaxTP(i),TPI2(i)]=min(xbl1(Ra(i):si2e(i)));%  end;
  end; S2(k)= Ra(i)+ ps2(k);  Tv= 0.18; 
 if ( Q1(k)== Q2(k) ) ,   Q(k) = Q1(k);
elseif (Q2(k) < Q1(k) ),   [MVqq(k),mvp(k)]=max(xbl1(Q2(k):Q1(k))); 
 if (MVqq(k) > ( xbl1(Q1(k)) + Tv)) ,   Q(k)=Q2(k);
elseif (xbl1(Q2(k))> xbl1(Q1(k))) ,    Q(k) = Q1(k); 
 else Q(k) = Q2(k); end;   else Q(k) = Q1(k);    
 end;   if  (S1(k) == S2(k)),   S(k) =  S1(k); 
 elseif ( xbl1( S2(k)) >xbl1( S1(k) )), S(k) =  S1(k); else S(k)= S2(k); %end;      
 end ;  if  ( xbl1( Ra(k ) )  < 0 ) ,  S(k)= S2(k);  end; 
%SO = S;S(SMnanN) = SM(SMnanN);% end;% for  i= 1 : RL  k = i;
TPR(k)  = S(k) + TPRANGE ;  % TPRJ(k)=  si2e(k)  +5 ;  
% if S(k )<NS-10 ,  FFRR2NDUR(K3 ) =  (Ra(z+2) - Ra(z+1) );
 if (TPR(k) > NS),  TPR(k) = NS; end ;
[TPVX(k), TPIX(k)]=  max( xbl1( S(k) : TPR(k) ) );
Tpx(k) = S(k) + TPIX(k) ; if Tpx(k)>NS, Tpx(k)=NS-6; end;%  end;
if ( Ra(k) > S(k)),if exist('SM','var') && (Ra(k)<SM(k)), S(k) = SM(k) ;   
  else   S(k) = Ra(k) + 10 ;  
end;  end;  if  ( k <  RL)  % &&  exist('SO','var')   
  [TPVD(k),TPID(k)]=max( xbl1(S(k) : TPR(k) )); a= xbl1(Ra(k) : S(k) );  
 Tpd(k)= S(k)+ TPID(k); [dtp2 p2 ] =min(abs(a - xbl1(Tpd(k))) );
  % text(Tpd(k),xd1(Tpd(k)),'Tp','color','r'); % a = xbl1( Ra(k): S(k));
 [dtp p]=min( abs(a -xbl1(Tpx(k)))); Rp1(k)= Ra(k)+p; %else Rp(k)=Ra(k)+3;end;
 prp2 = find( xbl1(Ra(k):S(k)) >=  xbl1(Tpx(k))+.2,1, 'last' );
 if  isempty(prp2) ,   prp2=p; end;    Rp2(k)= Ra(k)+ prp2;
if 0,prp(k)=find(xbl1(Ra(k):S(k))<=xbl1(Tpx(k)),1,'first');Rp3(k)=Ra(k)+prp(k);end; 
 NOT= find( xbl1(Ra(k) : S(k)-2) <=  xbl1(Tpx(k)),1,'first');
 % SS(k)=S(k); QQ(k)=Q(k); %   text(Rp1(k),xbl1(Rp1(k)),'Rp1','color','r');
  else Rp2(k)= NS ; 
end; 
GP=center2([Q(k) xbl1(Q(k))],[Ra(k) xbl1(Ra(k))],[ S(k) xbl1(S(k))],'incenter');
 GP1(k) =GP(1,1);   GP2(k) =GP(2,1);  GP2nan(k) =  isnan(GP2( k ) ) ;
if GP2nan(k),Bnan=Bnan+1;
  GPC= center2([Q(k) xbl1(Q(k))], [S(k) xbl1(S(k))],  ...
[Ra(k) xbl1(Ra(k))], 'barycenter'); GPC1  =GPC(1,1); GPC2(k) =GPC(2,1);
GP2(k) =GP2(k-1) ;
end; GPRL( k )=xbl1( Ra(k)) -GP2( k );GPRLN(k) = isnan(GPRL( k ) ) ;
% if  exist('xr', 'var'),   xr( Q(k) :  ( S(k) ) ) =0; end;
if (k+1) < RL && GPRLN(k)  ,
fprintf(' 1150 k=%d   Q=%d R=%d   S=%d Tp=%d Ra(k+1)=%d\n', ...
 k , Q(k), Ra(k),S(k), Tpx(k), Ra( k+1) ); %0.166*360= 60%0.083*360=30
end; end; if 10, hdom=figure('NAME', ['1150' title2 string ] , 'Position',...
 [25 screen(4)/2-80 screen(3)/2-30 screen(4)/2-30]); 
h = gcf ;  set(h,'WindowStyle','docked') ;  
 if (Afilemit ==101), De=540; start =1; heightPe=0.34;%S=S-12; hold on; 
elseif (Afilemit ==232), De=960; start =360; heightPe=0.34;%S=S-12;hold on; 
else   De=540; start =1; heightPe=0.34; 
end; if  exist('xbl1', 'var'), 
    plot( xbl1( start : De ) , 'b' , 'LineWidth', 2  ); 
minxbf1=min(xbl1(start : De))-0.1;  maxxbf1=max( xbl1(start :De))+0.1;
if 0,axis([start,(start+De),min(xbl1(start:DN))-0.1,max(xbl1(start:DN))+0.1]);end;
if  0&&exist('SM','var'), title2 =[' Q R S detection Record ', Afilemits];  
 SMnanN=find(~isnan(SM) ) ;  S(SMnanN) = SM(SMnanN) ;
end;
fy2 =[ minxbf1 0 maxxbf1]; 
set(gca , 'FontUnits' ,  'points', 'FontWeight' ,  'normal' , 'FontSize' , fonts16, ...
  'FontName', 'Times' , 'Units',  'normalized' ); 
for i= 1: length( fy2 ) [ STxbf1{ i,1} , errsg] = sprintf('%.2f',  fy2(i) ); end;  
set( gca, 'YTick', fy2 ) ; set( gca,'YTickLabel',  STxbf1 ); ylim( [ minxbf1 maxxbf1]);  
end;  xlim( [ start  De ] ) ; xticks= [ start-1: De/3:De]
set( gca,'XTick', xticks  ) ;  set( gca,'XTickLabel',xticks ) ; 
if  exist('xbl1','var') &&0,plot(xbl1  ,'g' , 'LineWidth', 2 ); hold on;  end; 
if exist('xd1', 'var') &&0,  plot(xd1, 'r' , 'LineWidth', 2 ); hold on;  end; 
line( [ start, DISP] , [ 0,0], 'Marker' ,  '.'  , 'Color', 'r'  );
MVqq=[];  mvp =[];   
 xlabel(' Time in samples' , 'FontSize' , fonts18  , 'Color', 'b'  );  
 ylabel('Amplitude ' , 'FontSize' , fonts18  , 'Color' , 'b'  ); 
 title2 =[ '  Q R S detection Record ', Afilemits ];  
 title( title2 , 'color', 'r' ,  'FontSize', fonts18 );  grid off;
[DB3 ]= find(Ra>=start & Ra <= De) ; DRT3 =Ra(DB3); 

for k=1: length(DRT3)  
if  Afilemit<400,string19q{k,1 }=[Ca(k) num2str(Ra(k))];
else  string19q{k,1 }=[num2str(Ra(k))];
end;  if (RR(k)<=De),    %  if DBUG,  dbstop 1184; end;  
   text( RR(k)+3,  xbl1(RR(k)+3), 'R'   , 'color', cb , 'FontSize',16 );  
    if 0,  text( Rp2(k), xbl1(Rp2(k)),  'Rp2', 'color', 'b' ); 
text( Ra(k)+3, xbl1( Ra(k)+3 )-0.0 , [  string19{k}]  ,'color', 'b' , 'FontSize',16 );        
end;if Afilemit==201| 1, if Q1(k)~= Q2(k)
 text(Q1(k)-0 , xbl1(Q1(k)-0)-0, 'Q1' ,'color', 'r' ,'FontSize',16);  
text(Q2(k)-0, xbl1(Q2(k)-0)-0.00, 'Q2' , 'color', 'r' ,'FontSize', 16 );  
    else      text(Q(k)-0, xbl1(Q(k)-0)-0.00, 'Q' , 'color', 'b' ,'FontSize', 16 ); 
    end;if S1(k)~= S2(k)
    text(S1(k)-0 , xbl1(S1(k)-0)-0.00, 'S1'   , 'color',   'r' ,  'FontSize', 16 );  
text( S2(k)-0 , xbl1(S2(k)-0 )-0.00, 'S2'   , 'color',   'r' ,  'FontSize', 16 );  
    else text(S(k)-0 , xbl1(S(k)-0)-0.00, 'S'   , 'color',   'b' ,  'FontSize', 16 );
    end; 
elseif Afilemit==101, text(Q(k)-1, xbl1(Q(k)), 'Q' , 'color', 'r' ,'FontSize',16 );
 text(S1(k)-2 , xbl1(S1(k)-2)-0.01, 'S1'   , 'color',   'r' ,  'FontSize', 16 );  
text( S2(k)-2 , xbl1(S2(k)-2 )-0.01, 'S2'   , 'color',   'r' ,  'FontSize', 16 );  
else  text(Q1(k)-0, xbl1(Q1(k))-0, 'Q1' ,'color', 'r' ,'FontSize',16);  
text(Q2(k)-0, xbl1(Q2(k)-0)-0.00, 'Q2' , 'color', 'r' ,'FontSize', 16 );  
text(S1(k)-0 , xbl1(S1(k)-0)-0.00, 'S1'   , 'color',   'r' ,  'FontSize', 16 );  
text( S2(k)-0 , xbl1(S2(k)-0 )-0.00, 'S2'   , 'color',   'r' ,  'FontSize', 16 );  
end; end; end;   RRAT = RRATSUM / RL;  dates
Tp=Tpx; Rp=Rp2;  [  zQI] = find(Q<= 0)  
NanGPRL = find( isnan(GPRL ) )  ;  GPRLNan=  GPRL(NanGPRL)
FIGFILE= [  'hdom' Afilemits dates B '.png'] ; 
Fhdom = fullfile(PATFIG, FIGFILE); if  DBUG, dbstop 1212; 
saveas( hdom , [ Fhdom ]  ) ;
end; end; if 10,titledeamp = ['Red Deamplified,  magenta Wander Removed'] ; 
U=start; V= DN ;     max1525= max(xbl1(start: DN))  ;   
min1180 = min(  min(xbl1( U : V)  ) , min(xbl1( U : V)  ) ) ;   
if   isempty( GPRLNan)  B2='NANNIL'   ; else B2='NANGP'; end;
NanGPRLRa= Ra(NanGPRL) ; BNAN = int2str( length( NanGPRLRa) )
z0minksR=[]; AC0= [  ]; AC= [  ];  clear   GP1 GP xhf xnf a b a1 b1 ;  
clear MVqq   Rp1 TPIX q1valmin q2valmin si2e  xlf1 xlf2   s2valmin; 
clear Tpx TPVX s1valmin  TPID TPVD Tpd  mvp pq1 pq2 ps1 ps2 prp2;
%Ra=Ra+3; RO= RO+3; set(0,'FontSize',18); %set(gcf,'FontSize', fonts18)
end; if 0, hl9 =figure( 'Name', [ '1210  QRS removed' ]   ...
,'Position',[10 30   SCR(3)/2-30  SCR(4)/2-30]) ;
set(gca,  'Units','normalized', 'XTick', U:fix((V-U)/2):V ,  ...  
 'YTick', min1180:(max1525- min1180)/2: max1525 ,  ...
 'FontUnits',  'points',  'FontWeight',  'normal', ...
 'FontSize', fonts16 , 'FontName', 'Times') ;% Pb2%170/360=0.4722
 plot(xr(1:DN) ,  'r'  , 'LineWidth', 2 ); 
 if (start ==1),plot( xbl1(1:DN) ,  'b'   , 'LineWidth', 2  ); 
axis([start, (start+DN),min(xbl1(start:DN) )-0.1,max(xbl1(start:DN))+0.1]); 
end;   for i=1:  length( DRT )   if Q(i) > 0, 
   text(Q(i), xbl1(Q(i))-0.1, 'Q' , 'color','r' ,'FontSize', 20  );
end;  if Rp(i), text(Rp(i), xbl1(Rp(i)), 'Rp' , 'color','r' ,'FontSize', 20 );
end; if (S(i) > 0), text(S(i), xbl1(S(i)),  's'    , 'color', 'r' ,'FontSize', 20 );
end;  if  exist('QM', 'var'),  if QM(i) > 0 &&~isnan(QM( i)), 
  text(QM(i), xbl1(QM(i)), 'QM' );  
 end; if (SM(i)> 0) &&  ~isnan(SM( i ) ), text(SM(i), xbl1(SM(i)),  'SM'  ); 
end; end; if exist('xbl1', 'var'), text(DRB(i), xbl1(DRB(i)),string19{i},'color','g'); 
end;  if  exist('xd1', 'var')  text(Ra(i), xd1(Ra(i)), 'R', 'color','r'   ); 
end;   if  exist('xbl1', 'var') , text( Tpx(i) , xbl1(Tpx(i)), 'Tp','color' ,'b'  );  
end;  end;   title( ['QRS removed '],'color', 'r' ,  'FontSize' ,16);
line( [ 0,DISP] , [ 0,0 ] , 'Marker' ,   '.' , 'Color', 'r'  ); grid off;  
if (RR(k)<=DN), text(S(k)-2, xbl1(S(k)-2), 'S', 'color', 'r' ,'FontSize', 16 );  
text(Q(k)-1,xbl1(Q(k)-1 )-0.01, 'Q' ,'color', 'r' , 'FontSize', 16 );  
text( RR(k)+2,xbl1(RR(k)+2),   'R'  , 'color' , 'r' ,'FontSize', fonts18 );  
end; end; end; if 17, if 1, hpoff= figure('NAME', [ '1230 Poff '...
 'So Chan Slope ' Afilemits uC ] , 'Position',  ...
[3  4  screen(3)/1-90 screen(4)/1-90]); h =gcf ;set(h,'WindowStyle','docked'); 
xbl3=  xbl1+xbl2; DISP=740;  D720=720; zPeI=[]; zPe= 0 ; 
if (Afilemit == 100), DISPe=540; start = 1; heightPe=0.34; %S= S-12;
elseif (Afilemit ==101),DISPe=440; start =1; heightPe=0.2;
 elseif (Afilemit==106),DISPe=720;start=270; Q(2)=Q(2)+(7);heightPe=0.34; 
 elseif (Afilemit==119),DISPe=540; start = 1 ;  heightPe=0.34; 
S(1)= S(1)-12;S(2)=S(2)-(7);   Q(2)= Q(2)+(7);  
 elseif (Afilemit==200),DISPe=540; start = 1 ;  heightPe=0.34;  
%Ra(1:3) = Ra(1:3)+2;RO(1:4)  = RO(1:4)+2  ;
elseif ( Afilemit  ==202 ),  DISPe=760; start = 1; heightPe=0.34;
    
elseif ( Afilemit==205)||( Afilemit ==228),DISPe=700; start =1; heightPe=0.34; 
elseif ( Afilemit==207), start= Q(1)-100; DISPe= Q(4)+100;  
    
elseif ( Afilemit==222), DISPe=740;  start =1;     
   elseif ( Afilemit ==232 ),  DISPe=740; start = 1;  heightPe=0.34; 
   elseif ( Afilemit  ==233 ),  DISPe=740; start = 1;  heightPe=0.34;   
 else  DISPe=740; start = 1 ;  heightPe=0.34; 
end; if ~exist('heightPe', 'var'), heightPe=0.34; end;
 if  start<1, start=1; end;  DRa=find(Ra>=start &Ra<=DISPe);DRe=Ra(DRa);   
plot( (start:DISPe) , xbl1(start:DISPe) , cb  , 'LineWidth', 2  );
minxbf17=min(xbl1(start:DISPe))-0.2;maxxbf17=max(xbl1(start:DISPe))+heightPe;  
CELLf1= minxbf17:(maxxbf17- minxbf17)/2: maxxbf17 ;
 B1=[  start-1:floor((DISPe-start+1)/2):DISPe ] ;
 set(gca,  'Units','normalized',   'FontUnits',  'points', ... %'XTick', B1 , ...  
  'FontWeight',  'normal' ,'FontSize', fonts16 , 'FontName', 'Times' ) ; 
line( [ start-1,DISPe] , [ 0 ,0 ] , 'Marker' ,  '.'  , 'Color', 'r'  ); 
title2 =[ ' Poffset search range using Q, Record  ', Afilemits]; 
 title( title2 , 'color', 'r' ,'FontSize', fonts18); ylim([ minxbf17  maxxbf17 ]) ;   
for i =1: length(CELLf1)  [ CELLf1s{ i,1}, cB2] =sprintf('%.1f', CELLf1(i)); end;
 set(gca,'YTick', CELLf1 ) ; set(gca, 'YTickLabel',CELLf1s ); 
 if Afilemit ==202, B1=[  start-1 floor((DISPe+start+1)/2) DISPe ] ;
 else B1=[start-1:fix((DISPe-start)/2):DISPe+1]; xlim([ start-11 DISPe]);
 end; set( gca,'XTick', B1) ;set( gca,'XTickLabel', B1); xlim([ start-11 DISPe]);
 for i=1: length(DRe) text(S(i )-15 , xbl1(S(i)-5)-0.03 , 'S' , 'color','r' ,'FontSize',15);
 if Q(i) > 0,  text( Q(i) , xbl1(Q(i))-0.01, 'Q' ,'color','r' ,'FontSize', 13  ); end; 
if 0,  text(Tp(i), xbl1(Tp( i ) ) , 'J'   , 'color',  'r' );   end; 
 if 0, text(Ra(i),xbl1(Ra(i)+2 ), [int2str(i) string19{i} ] , 'color', 'r','FontSize',16);end;
 text(Ra(i)+2,xbl1(Ra(i)+2)+0.04,[int2str(i) 'R' int2str(Ra(i))],'color','r','FontSize',16);  
end;  [xmax,  xmaxin ]= max( xbl1);  clear xu;uC
fprintf(' 1205  Pe2f  So Chan Slope  cof1= %f cof2 =%f \n', cof1, cof2);
clear xlogic1 xlogi1  xlf1 xlf2  xd1 xd2 ; 
 xlabel( 'Time in seconds ' ,'FontSize',19, 'Color', 'b' );xlim([ 1 DISPe]);
  xlabel(' Time in samples' , 'FontSize' , fonts18 , 'Color', 'b'  );  
 ylabel(' Amplitude (mV)', 'FontSize', 18, 'Color', 'b');  
 [slomax250 ,slomaxi]= max(slopes(3 , 250 , xbl1));  maxi= slomax250;
pept_param = 0.2;  pefilter_param =4; pethresh_param= 2; %2 4, 8 16;  
peslope_thresh =(pethresh_param/16 )*maxi ;
pept_thresh = pept_param* peslope_thresh; % hpoffa = figure(hpoff ) ; 
%f=1; first_max= xbl1(Ra(f)) - xbl1(Q(f));maxi=((first_max-maxi )/4)+maxi ; 
if  DBUG, dbstop 1297; end; %h = gca; set(hpoff,'CurrentAxes', h); 
end; if 1, for f  = 1 :  RL   if 1,%Pe2f
 if (Afilemit  ==100 ),   PeS(f) = Q(f)-30 ; PeRange=20 ;  
elseif ( Afilemit ==  101 ),   PeS(f) = Q(f)-8 ; PeRange= 30 ;  
 elseif ( Afilemit  ==  103 ),   PeS(f) = Q(f) -1 ; PeRange=30 ;   
elseif ( Afilemit  ==  105 ), PeS(f) = Q(f)-1 ; PeRange=30 ;
elseif ( Afilemit  ==  106 ) , PeS(f) = Q(f)-0 ;  PeRange=25 ;   
elseif ( Afilemit  ==  108 ), PeS(f) = Q(f)-15;    PeRange=25 ;   
  elseif ( Afilemit  ==  109 ), PeS(f) = Q(f)-0  ; PeRange=25;   
      elseif ( Afilemit  ==  111), PeS(f) = Q(f)-3 ;  PeRange= 20;  
      elseif   ( Afilemit  ==  112 ), PeS(f) = Q(f) -1 ;  PeRange= 15 ;  
      elseif   ( Afilemit  ==  113 ), PeS(f) = Q(f) -1 ;  PeRange=20 ;
      elseif   ( Afilemit  ==  114 ), PeS(f) = Q(f)-2;  PeRange=30 ;   
      elseif   ( Afilemit  ==  115 ), PeS(f) = Q(f) -2 ;  PeRange= 30 ;
 elseif   ( Afilemit  ==  116 ), PeS(f) = Q(f)-0;  PeRange=20;
if  isequal( Ca(f) , 'N' ) , PeS(f) = Q(f) -0 ;   PeRange= 26 ; end;        
      elseif  ( Afilemit  ==  117  ) , PeS(f) = Q(f)-2 ; PeRange=30 ;
      elseif ( Afilemit  ==  118 ), PeS(f) = Q(f)   ;  PeRange= 20 ;  
      elseif ( Afilemit  ==  119 ),  PeS(f) = Q(f)-0 ; PeRange=20 ;
      elseif ( Afilemit  ==  123 ),     PeS(f) = Q(f)-0 ;   PeRange=25 ;
  elseif ( Afilemit  ==  124 ), PeS(f) = Q(f)-0;  PeRange=24;       
 elseif  ( Afilemit  ==  200 ), PeS(f) =  Q(f)+1 ; PeRange= 25 ;
 if  isequal(  Ca(f) , 'V' ) , PeS(f) = Q(f)-0 ;   PeRange= 2 ;     end;
 if  isequal(  Ca(f) , 'N' ) , PeS(f) = Q(f) -0 ;   PeRange= 6 ;     end;
 if  isequal(  Ca(f) , 'A' ) , PeS(f) = Q(f) -0 ;   PeRange= 6 ; end;  
elseif ( Afilemit  ==  201 ),   PeS(f) = Q(f) ;  PeRange=12;
if 0, if  isequal(  Ca(f) , 'A' ) , PeS(f) = Q(f) - 0 ; PeRange= 2;   end;
 if  isequal(  Ca(f) , 'V' ) , PeS(f) = Q(f)-1 ;   PeRange= 2 ;     end;
if  isequal(  Ca(f) , 'j' ) , PeS(f) = Q(f) -1 ;   PeRange= 2 ; end;
if  isequal(  Ca(f) , 'a' ) , PeS(f) = Q(f)-1 ;   PeRange= 2 ;     end;
 if  isequal(  Ca(f) , 'N' ) , PeS(f) = Q(f) -1 ;   PeRange= 2 ;     end;
end;   elseif ( Afilemit  ==  202 ) , PeS(f) = Q(f)- 0 ; PeRange=20 ; 
 elseif ( Afilemit  ==  203 ) ,   PeS(f) = Q(f);    PeRange= 25 ;      
  if  isequal(  Ca(f) , 'N' ) , PeS(f) = Q(f)  ;   PeRange= 8 ;     end;    
      elseif ( Afilemit  ==  205 )   PeS(f) =Q(f)+1;    PeRange=15;   
          
elseif ( Afilemit  ==  207 ), PeS(f) = Q(f)-3  ;   PeRange=30 ;%AELRV
 if  isequal(  Ca(f) , 'E' ) , PeS(f) = Q(f) -2  ;   PeRange= 40 ;  end;        
       if  isequal(  Ca(f) , 'L' ) , PeS(f) = Q(f) -2  ;   PeRange= 30 ;  end;     
        if  isequal(  Ca(f) , 'V' ) , PeS(f) = Q(f) -2  ;   PeRange= 30 ;  end;     
 elseif ( Afilemit  ==  208 ) , PeS(f) = Q(f)-0 ;  PeRange=25 ;   
 if isequal(  Ca(f) , 'S' ) , PeS(f) = Q(f) + 2 ;   PeRange= 12 ; end;
 if  isequal(  Ca(f) , 'V' ) , PeS(f) = Q(f)-0 ;   PeRange= 5 ;  end;  
 if f== 127,PeS(f) = Q(f)-0 ;   PeRange= 1 ;  end;  
  if f== 1803,PeS(f) = Q(f)-0 ;   PeRange= 1 ;  end;  
 elseif ( Afilemit  == 209 ) ,  PeS(f) = Q(f)- 0 ;   PeRange=20;         
 elseif ( Afilemit  ==  210 ) ,    PeS(f) = Q(f)+0 ;   PeRange=  15 ;
    if  isequal(  Ca(f) , 'E' ) , PeS(f) = Q(f) + 0 ;   PeRange= 12 ;  end;                     
   elseif ( Afilemit  ==  212 ) ,    PeS(f) = Q(f)+0 ;   PeRange=  15 ;   
 elseif ( Afilemit  ==  213 )  PeS(f) = Q(f) + 2 ;  PeRange=  5 ;
 if  isequal(  Ca(f) , 'V' ) , PeS(f) = Q(f) + 4 ;   PeRange= 2 ;    end;
 if  isequal(  Ca(f) , 'A' ) , PeS(f) = Q(f) + 4 ;   PeRange= 6 ;   end;
  elseif   ( Afilemit  ==  214 )  ,  PeS(f) = Q(f)  +2 ; PeRange= 5 ;
if  isequal(  Ca(f) , 'V' ) , PeS(f) = Q(f) + 4 ;     end;
if  isequal(  Ca(f) , 'L' ) , PeS(f) = Q(f) + 4 ;     end;
if  isequal(  Ca(f) , 'F' ) , PeS(f) = Q(f) + 4 ;     end;  
elseif   ( Afilemit  ==  215 ), PeS(f) = Q(f) -1 ;  PeRange=27;
    
  elseif  ( Afilemit  ==  217 ), PeS(f) = Q(f) + 14 ;  PeRange= 55 ;  
     elseif  ( Afilemit  ==  219 ), PeS(f) = Q(f);  PeRange=20;         
      elseif   ( Afilemit  ==  220 ), PeS(f) = Q(f) + 1 ;  PeRange=45 ;
      elseif   ( Afilemit  ==  221 ), PeS(f) = Q(f) +4 ;  PeRange= 25 ;
      elseif   ( Afilemit  ==  222 ), PeS(f) = Q(f)+2 ; PeRange= 6 ; 
 if  isequal(  Ca(f) , 'A' ) , PeS(f) = Q(f)   ;   PeRange= 6 ;   end;
 if  isequal(  Ca(f) , 'J' ) , PeS(f) = Q(f)   ;   PeRange= 6 ;     end;
 if  isequal(  Ca(f) , 'j' ) , PeS(f) = Q(f)   ;   PeRange= 6 ;     end;   
  if  isequal(  Ca(f) , 'N' ) , PeS(f) = Q(f) + 0 ;   PeRange= 12 ;     end;
 elseif   ( Afilemit  ==  223 ),   PeS(f) = Q(f)  ; PeRange = 40 ;
  if  isequal(  Ca(f) , 'V' ) , PeS(f) = Q(f) ;PeRange= 12 ; end;
 if  isequal(  Ca(f) , 'A' ) , PeS(f) = Q(f);   PeRange= 6 ;     end;
  elseif   ( Afilemit  ==228 ), PeS(f) = Q(f) + 1;  PeRange= 20;
elseif   ( Afilemit  ==  230 ), PeS(f) = Q(f) + 0;  PeRange= 25 ;         
   elseif   ( Afilemit  ==  231 ), PeS(f) = Q(f) + 0;  PeRange= 25 ;     
 elseif   ( Afilemit  ==  232 ), PeS(f) = Q(f)+1;  PeRange= 20 ;
elseif ( Afilemit  ==  233 ), PeS(f) = Q(f)-1 ;  PeRange=20 ;
 if  isequal(  Ca(f) , 'V' ) , PeS(f) = Q(f) -1 ;  PeRange= 12; end;
 if  isequal(  Ca(f) , 'A') , PeS(f) = Q(f)-1;   PeRange= 10 ; end;      
elseif ( Afilemit  ==  234 ), PeS(f) = Q(f) ;  PeRange=20 ;  
if  isequal( Ca(f) , 'V' ) , PeS(f) = Q(f)   ;  PeRange=12 ; end;
if  isequal( Ca(f) , 'J' ) , PeS(f) = Q(f)  ; PeRange= 10 ; end;

else   PeS(f) = Q(f)-1  ; PeRange=  30 ;
end; pf(f)= 0; j=1;  PeRange2(f) =PeS(f)- Q(f)+PeRange ;  
first_max=xbl1(Ra(f))-xbl1(Q(f));maxi=((first_max-maxi)/pefilter_param)+maxi;
 peslope_thresh =( pethresh_param/16 )*maxi ; 
 pept_thresh = pept_param * peslope_thresh ; 
PeSQ(f) = Q(f)- PeS(f) ;   PeSQ2(f) =  PeS(f)- Q(f) ;
end; for i = PeS(f) : -1: PeS(f)-PeRange %Pe2f
%  for i =  PeS(f) : -1 :  Q(f)-PeRange   
if i <=0, pf(f)=pf(f)+1;  Pe2f(f)= Q(f)-1; break;  end;
 if ( abs(slope(i,xbl1)) >=pept_thresh  ) ...%&& (xbl1(i)==0)
  &&(abs(slope(i-1,xbl1))>=pept_thresh ), Pe2(f,j) =i-1;  j = j+1; 
 %fprintf('i=%df=%d %cQ=%dRa%d %d\n',i, f,C(f),Q(f), Ra(f), S(f));
break; end; 
end;  if 1, if (j>1), Pe2f(f)=Pe2(f,j-1);
else  Pe2f(f)= Q(f); zPe=zPe +1 ; zPeI=[ zPeI f ];   
end; if  (Q(f)-Pe2f(f) )<=0, Pe2fQ(f) =3; else Pe2fQ(f) = -Pe2f(f)+Q(f) ;
end; if ( f <= length(DRe)) ,
if (Pe2f(f)>start && Pe2f(f)<1000), fprintf('1400 f=%d pe=%d\n', f, Pe2f(f)) ;    
 text( Pe2f(f),xbl1( Pe2f(f ))-0.2, ['\uparrowPoffset' ] ,'color','r','FontSize',18);  
 end;  fmp2(f,:)= xbl1(Ra(f))+xbl1(Pe2f(f)) ; if DBUG dbstop 1397; 
 end;   xuc(f,:) = [ Pe2f(f), Q(f), Pe2fQ(f),   PeSQ(f)  ] ;
 xu1(f,:) = [ Pe2f(f), xbl1(Pe2f(f)), PeRange2(f), xbl1(Pe2f(f))+0.5 ] ;
 if 0, text(Pe2f(f),xbl1(Pe2f(f)),['Qlag=' int2str(PeSQ(f))], 'color','r','FontSize', 16); 
text( Pe2f(f) , 0.6 , [ 'Search range from Q-Qlag' ] ,'FontSize', 18 , 'color', cr );  
 end;  rectangle( 'Position', [ Pe2f(f), xbl1(Pe2f(f)) , Pe2fQ(f), 0.34],  ...
 'EdgeColor', cr ,'LineWidth', 2  ) ;
%text(Ra(f),xbl1(Ra(f)+2 ), [int2str(f ) string19{f} ] , 'color', 'r' ,'FontSize',16 );
if 0, text( Pe2f(f)-70, 0.5, [ 'Search range ' ], 'FontSize', 18 , 'color', cr );
else  text( Pe2f(f)-40, 0.5, [ 'Search ' ], 'FontSize', 18 , 'color', cr );
  text( Pe2f(f)-40, 0.4, [ 'range ' ], 'FontSize', 18 , 'color', cr );   
end; text( Pe2f(f)+0 ,  0.4, [ '\leftarrow' ] , 'color',  cr  , 'FontSize', 27 ); 
 if 0, if ( S(f )>0) && (~isempty(S1) &&S(f )< DISPe),
% text( S(f )-0 , xbl1(S(f )-0 )-0.03 , 'S' , 'color','r' ,'FontSize' , 15 );     
text( S(f )-15 , xbl1(S(f )-5)-0.03 , 'S' , 'color','r' ,'FontSize' , 15 );  
end; if Q(f) >0,  text( Q(f) , xbl1(Q(f))-0.01, 'Q' ,'color','r' ,'FontSize',13 ); end; 
end; end; 
end; end;   if 1,  [ zlPe2f ] =find (Pe2f<=0)
 if Afilemit ==202, B1=[  start-1 floor((DISPe+start+1)/2) DISPe ] ;
 else B1=[start-1:fix((DISPe-start)/2):DISPe+1]; xlim([ start-11 DISPe]);
 end; set( gca,'XTick', B1) ;set( gca,'XTickLabel', B1); xlim([ start-11 DISPe]);
%text( Pe2f(1), 0.6 , [ 'Search range ' ] ,'FontSize', 18 , 'color', cr  );  
[pept_threshs,er]=sprintf('%.3f',pept_thresh); pept_thresh3=str2num(pept_threshs);
[PeRange2max PeRange2maxI ]=max(PeRange2);
[Pe2fQmax Pe2fQmaxI ]=max(Pe2fQ); [PeSQmax PeSQmaxI ]=max(PeSQ);
 PeRange2min =min( PeRange2 ); Pe2fQmin=min( Pe2fQ ); 
dates=[ d(10:11),d(4:6),d(1:2),d(13:14),d(16:17) ];
zlPe2fl = length(zlPe2f );   zlPe2fR= Ra(zlPe2f) ; Ra3= int2str(Ra(1:4)' )
 fprintf(' 1425 Pe2f zlPe2f%d \n', zlPe2fl); ErrorI=[];zlPe2fca= Ca(zlPe2f) 
 for i = 1 : length( zlPe2f)  [ ErrorI(i) ] = find( RO == Ra(zlPe2f(i) )); end;
 clear fm1 fm2 filsig  fy ecg1  NORMR ;  zlPe2fCp1= C(ErrorI +1);
hapon= get( gcf , 'CurrentAxes') ; % set( gcf , 'InvertHardCopy' , 'off' ) ; 
FIGFILpo=['Poff' Afilemits dates B '.png']; Fhpoff =fullfile(PATFIG,FIGFILpo) ;
wpbe = [PeSQmax  Pe2fQmax  PeRange2max pept_thresh ];
if  DBUG, dbstop 1431;datest=[ d(10:11), d(4:6),d(1:2) ];
saveas( hpoff , [ Fhpoff  ] ) ;
end; end;end; if 1, hPon=figure( 'Name', [Afilemits '1430 Ponset' Ra3 uC], ...
'Position',[10 30 SCR(3)-100 SCR(4)/1-120]);h=gcf; set(h,'WindowStyle','docked'); 
if ( Afilemit==200), UPon= 1; VPon= 740 ;  
elseif ( Afilemit==202 ), UPon= Q(1 )-100; VPon= Q(2 )+100;   
elseif ( Afilemit==207), UPon= Q(1 )-100; VPon= Q(4 )+100;  VPon=500;
  elseif ( Afilemit==101), UPon= Q(1 )-100; VPon= Q(4 )+100;  VPon=500; 
elseif ( Afilemit==111), UPon= Q(1 )-100; VPon= Q(4 )+100;  VPon=500;     
else  UPon=1; VPon= 740; end; if  UPon<=0,  UPon=1 ; end;  
minxbf1p=min(xbl1(UPon:VPon ))-0.2; xlim([ 1 DISPe]);
maxxbf1p=max(xbl1(UPon:VPon))+heightPe;  Pb1=[]; start1=1;zP= 0 ;   
set(gca, 'Units', 'normalized',  'FontUnits',  'points',     ... 
 'FontWeight',  'normal', 'FontSize', 17 , 'FontName', 'Times' ); 
if  exist('xbl1','var'), plot( (UPon:VPon), xbl1(UPon:VPon), cb, 'LineWidth', 2); 
line( [ UPon, VPon+0] , [ 0, 0 ] , 'Marker' ,  '.'  , 'Color', 'r'  );  
DRd=find(Ra >= UPon & Ra <= VPon);  DR=Ra(DRd);  grid off;    
GPC2= [  UPon  fix((VPon-UPon+1)/2)  VPon ] ;
 A3 = [ UPon-1  floor((VPon+20-UPon-0)/2)   VPon+00 ] ;
set( gca,'XTick', A3 ) ;  set( gca,'XTickLabel', A3) ; hold on; 
xlim( [  UPon-1  VPon+0  ] ) ;   ylim( [ minxbf1p maxxbf1p] ); 
ampl =[  minxbf1p: (maxxbf1p- minxbf1p)/2  : maxxbf1p ] ;
set(gca,  'Units', 'normalized',  'FontUnits',  'points',       ... 
 'FontWeight',  'normal', 'FontSize', 17 , 'FontName', 'Times' ); 
for i= 1: length( ampl)  [ cs935{ i,1} , ccTB2] = sprintf('%5.2f',ampl(i)); 
end;  set(gca, 'YTick', ampl ); set(gca, 'YTickLabel', cs935);  
 ylabel(' Amplitude, mV' ,   'FontSize' , fonts18  , 'Color', 'b' ); 
 xlabel( 'Time in samples 360 samples per sec', 'FontSize',18 ,'Color', 'b'); 
end;  xdiff = [ ' Green wander Removed, Blue Bpass filtered' ] ; 
title2 = [ 'Ponset Search range using Poffset, Record  ', Afilemits ];  
 title(  title2 , 'color' , 'r' ,  'FontSize',  16 ) ;dbstop 1632;   uC
 if ( Afilemit== 118 ),  start1=2;  PbR = 65; else PbR =45; end; uC 
% set( hPon, 'CurrentAxes', hapon ) ;%left to right 
 minpb = min( min( xbl1(start:DISP))-0.1 , -1 ); pb_param = 0.1 ;
maxpb = max( max( xbl1(start:DISP))+0.1 ,1 ); thresh_pb= 2 ;  
thresh_pb = 2;  pb_param= 0.1; f=1; first_max= xbl1( Ra(f))- xbl1( Q(f)); 
maxi = ((first_max- maxi)/4)+maxi; hPona= figure( hPon ); uC
if 0, set( gcf , 'color', cb ) ;  set( gca , 'color', cm ) ;
defaultBackground = get( 0, 'defaultUicontrolBackgroundColor' );     
set( hPon , 'Color' , defaultBackground  ) ;   dbstop 1473;  
end; 
end;end;  for f = 1 : RL    j=1;  if  1, %Pb2 
 if ( Afilemit == 100),  PbR =65;
 if  (Pe2f(f) >=1),   PbS(f)=  Pe2f(f) - PbR;  PbE(f)= Pe2f(f) ;
 else   PbS(f)= Q(f) - PbR ;  PbE(f)= Q(f) ;     end;
elseif ( Afilemit  == 101), 
    if  (Pe2f(f) >= 1),  PbS(f)= Pe2f(f) -PbR;  PbE(f)= Pe2f(f)  ; 
else  PbS(f)=  Q(f) - 156;  PbE(f)= Q(f)- 56;   end;           
    elseif ( Afilemit  == 103),
 if  (Pe2f(f) >=1),   PbS(f)=  Pe2f(f) -PbR;  PbE(f)= Pe2f(f); 
 else   PbS(f)=Q(f) - PbR;  PbE(f)= Q(f); end;
elseif (Afilemit==104), if (Pe2f(f)>=1),   PbS(f)=Pe2f(f) -PbR;
 PbE(f)=Pe2f(f);  else  PbS(f)=  Q(f) - 156;  PbE(f)= Q(f)- 56;   end;
    elseif ( Afilemit==105),if  (Pe2f(f) >= 1), PbS(f)= Pe2f(f) -PbR ;    
PbE(f)= Pe2f(f) -  0 ; else  PbS(f)=  Q(f)-156;  PbE(f)= Q(f)- 56; end;
elseif ( Afilemit== 106), PbR =50;if  (Pe2f(f) >=1), PbS(f)= Pe2f(f)-PbR ;    
PbE(f)= Pe2f(f) - 10 ; else  PbS(f)=  Q(f) -156;  PbE(f)= Q(f)- 56; end;
   elseif ( Afilemit  == 108), PbR =53;
 if  (Pe2f(f) >= 1), PbS(f)= Pe2f(f) -PbR ;   
PbE(f)= Pe2f(f)-1;else  PbS(f)=  Q(f) - 156;  PbE(f)= Q(f)- 56;    end;
elseif ( Afilemit  == 109  ),  
if  (Pe2f(f) >= 1),  PbS(f)= Pe2f(f)-PbR ; PbE(f)= Pe2f(f) -0 ;
else  PbS(f)=  Q(f) - 106;  PbE(f)= Q(f)- 56;    end;
if  isequal(  Ca(f) , 'L' ) , PbS(f)= Pe2f(f) - 14 ;  PbE(f)= Pe2f(f); end;      
elseif ( Afilemit ==111), PbS(f)=Pe2f(f)-45; PbE(f)= Pe2f(f)-2;

 elseif ( Afilemit  == 113  ),
     PbS(f)=  Pe2f(f) -PbR; PbE(f)= Pe2f(f)- 2;   
%  if  isequal(  Ca(f) , 'N' ) , PbS(f)= Pe2f(f) - 14 ;  PbE(f)= Pe2f(f);   end;    
   if  isequal(  Ca(f) , 'a' ) , PbS(f)= Pe2f(f) - 10 ;  PbE(f)= Pe2f(f);   end;   
   
 elseif ( Afilemit  == 114 ),PbR =50;
     PbS(f)=  Pe2f(f) - PbR; PbE(f)= Pe2f(f)- 2;    
  elseif ( Afilemit  == 115  ), PbR =55;
      PbS(f)=  Pe2f(f)-PbR; PbE(f)= Pe2f(f)- 2;       
 elseif ( Afilemit == 116),,PbR =50;
     if  ( Pe2f(f) >=1), PbS(f)=Pe2f(f)-PbR; 
PbE(f)= Pe2f(f)-2; else  PbS(f)=  Q(f) - 106;  PbE(f)= Q(f)- 56;  end;
if 0, if  isequal(Ca(f) , 'N' ) , PbS(f)= Pe2f(f) - 14 ;  PbE(f)= Pe2f(f); end; 
    if  isequal(  Ca(f) , 'A' ) , PbS(f)= Pe2f(f) - 14 ;  PbE(f)= Pe2f(f); end; 
end;   elseif ( Afilemit  == 117  ), PbR=40 ; 
   if  (Pe2f(f) >= 1),  PbS(f)= Pe2f(f) -PbR ; PbE(f)= Pe2f(f)-1 ;
    else  PbS(f)=  Q(f) - 106;  PbE(f)= Q(f)- 56;    end;
 elseif( Afilemit==118), PbR = 55;  PbS(f)= Pe2f(f)- PbR; PbE(f)= Pe2f(f) ;
start1=2; if  isequal(Ca(f) , 'V' ) ,PbS(f)= Pe2f(f) -10 ;  PbE(f)= Pe2f(f); end;
  if  isequal(  Ca(f) , 'A' ) ,PbS(f)= Pe2f(f) - 4 ;  PbE(f)= Pe2f(f);   end;
 if  isequal(  Ca(f) , 'R' ) , PbS(f)= Pe2f(f) - 4 ;  PbE(f)= Pe2f(f);     end;
 elseif ( Afilemit== 119),if (Pe2f(f)>=1), PbR=30;PbS(f)=Pe2f(f)-PbR; 
  PbE(f)=Pe2f(f); else  PbS(f)=  Q(f) - 156;  PbE(f)= Q(f)- 56;    end;
elseif ( Afilemit  == 123),if  (Pe2f(f) >= 1), PbS(f)= Pe2f(f)-PbR; 
PbE(f)= Pe2f(f)- 2; else  PbS(f)=  Q(f) - 156;  PbE(f)= Q(f)- 56;   end;  
elseif ( Afilemit == 124), if (Pe2f(f) >= 1), PbS(f)= Pe2f(f)-PbR;
PbE(f)=Pe2f(f); else  PbS(f)=  Q(f)-156;  PbE(f)=Q(f)- 56;   end; 
elseif ( Afilemit  == 200), PbR =55;  if  (Pe2f(f) >=1),  PbS(f)=Pe2f(f) - PbR ;   
PbE(f)= Pe2f(f) - 2; else  PbS(f)=  Q(f) - 156;  PbE(f)= Q(f)- 56;    end;
  if  isequal(  Ca(f) , 'a' ),PbS(f)= Pe2f(f)- 20; PbE(f)=Pe2f(f) ;   end;         
elseif ( Afilemit  == 201),PbR=40 ; 
    if (Pe2f(f) >=1), PbS(f)= Pe2f(f)-PbR ;
 PbE(f)= Pe2f(f) ; else  PbS(f)=Q(f) - 156;  PbE(f)= Q(f)- 56;  end;  
  if  isequal(  Ca(f) , 'a' ),PbS(f)= Pe2f(f)- PbR; PbE(f)=  Pe2f(f) ; end;
 
 elseif ( Afilemit  == 202),PbR=50 ; 
     if  (Pe2f(f) >= 1), PbS(f)= Pe2f(f)-PbR;  
 PbE(f)= Pe2f(f)  ; else  PbS(f)=  Q(f) - 156;  PbE(f)= Q(f)- 56;  end;    
if  isequal(Ca(f) , 'a' ) ,  PbS(f)= Pe2f(f)- 20 ; PbE(f)=  Pe2f(f) ;  end;
elseif ( Afilemit  == 203),PbR=50 ; 
    if  (Pe2f(f) >= 1), PbS(f)= Pe2f(f) - PbR ; 
PbE(f)= Pe2f(f)  ; else  PbS(f)=  Q(f) - 156;  PbE(f)= Q(f)- 56;   end; 
elseif (Afilemit == 205),PbR=35  ;  if  (Pe2f(f) >=1), PbS(f)= Pe2f(f)-PbR; 
PbE(f)= Pe2f(f); else  PbS(f)=  Q(f) - 156;  PbE(f)= Q(f)- 56; end; 
if  isequal(  Ca(f), 'N' ) ,PbS(f)= Pe2f(f) -PbR ;  PbE(f)= Pe2f(f);    end;

 elseif ( Afilemit  == 207 ), if  (Pe2f(f) >= 1), PbS(f)= Pe2f(f)-PbR ;   
PbE(f)= Pe2f(f)  ;  else  PbS(f)=  Q(f) - 156;  PbE(f)= Q(f)- 56;    end;   
if  isequal(  Ca(f), 'V' ) ,PbS(f)= Pe2f(f) -10 ;  PbE(f)= Pe2f(f);  end;
  if  isequal(  Ca(f) , 'A' ) ,PbS(f)= Pe2f(f) - 4 ;   PbE(f)= Pe2f(f) ;   end;
    if  isequal(  Ca(f) , 'E' ) , PbS(f)= Pe2f(f) - 4 ;  PbE(f)= Pe2f(f);     end;   
  elseif (Afilemit== 208),PbR=50 ;   if  (Pe2f(f) >= 1), PbS(f)=Pe2f(f)-PbR;
PbE(f)= Pe2f(f); else  PbS(f)=  Q(f) - 156;  PbE(f)= Q(f)- 56;       end;
if 10,  if  isequal(  Ca(f) , 'A' ) ,PbS(f)= Pe2f(f)-PbR;  PbE(f)= Pe2f(f);   end;   
    if  isequal(  Ca(f) , 'S' ) , PbS(f)= Pe2f(f)-PbR   ;  PbE(f)= Pe2f(f) ;end;     
 if  isequal(  Ca(f) , 'V' ) ,PbS(f)= Pe2f(f) -30   ;  PbE(f)= Pe2f(f) ;   end;   
  if  isequal(  Ca(f) , 'F' ) ,PbS(f)= Pe2f(f) -PbR  ;PbE(f)= Pe2f(f) ;end;    
end;  elseif (Afilemit ==209),
if  (Pe2f(f)>=1), PbS(f)=Pe2f(f)-PbR;PbE(f)=Pe2f(f)-2;     
  else  PbS(f)=  Q(f) - 156;  PbE(f)= Q(f)- 56;    
end; if  isequal(  Ca(f) , 'V' ) ,PbS(f)= Pe2f(f) - 10 ;  PbE(f)= Pe2f(f);  end;  
 if  isequal(  Ca(f), 'A' ) ,PbS(f)= Pe2f(f) - 14 ;  PbE(f)= Pe2f(f); end;
  if  isequal(  Ca(f) , 'S' ) , PbS(f)= Pe2f(f) - 14 ;  PbE(f)= Pe2f(f);  end; 
    if  isequal(  Ca(f) , 'N' ) , PbS(f)= Pe2f(f) -24 ;  PbE(f)= Pe2f(f);  end; 
elseif ( Afilemit  == 210  ),  PbR  =35;
 if  (Pe2f(f) >= 1), PbS(f)= Pe2f(f) - PbR  ;   PbE(f)= Pe2f(f)  ;
 else  PbS(f)=  Q(f) - 156;  PbE(f)= Q(f)- 56;   end;
 if  isequal(Ca(f) , 'E'), PbS(f)= Pe2f(f) - 14;  PbE(f)= Pe2f(f);  end;       
 if  isequal(Ca(f) , 'a'), PbS(f)= Pe2f(f) - 14;  PbE(f)= Pe2f(f);  end;       
 elseif ( Afilemit  == 212 ), PbR =30 ;
 if  (Pe2f(f) >= 1), PbS(f)= Pe2f(f) - PbR  ;   PbE(f)= Pe2f(f)  ;
     else  PbS(f)=  Q(f) - 156;  PbE(f)= Q(f)- 56;   end;
    elseif ( Afilemit  == 213 ),if  (Pe2f(f) >= 1), PbS(f)= Pe2f(f) -PbR ;  
 PbE(f)= Pe2f(f)-2 ; else  PbS(f)=  Q(f) - 156;  PbE(f)= Q(f)- 56; end;
  if 0,if  isequal(  Ca(f) , 'V' ) ,PbS(f)= Pe2f(f) - 45 ;  PbE(f)= Pe2f(f); end;
if  isequal(  Ca(f) , 'A' ) ,PbS(f)= Pe2f(f) - 14 ;   PbE(f)= Pe2f(f); end;
  if  isequal(Ca(f) , 'S' ) , PbS(f)= Pe2f(f) - 4 ;  PbS(f)= Pe2f(f) - 14; end;
 end; elseif ( Afilemit==214),PbR=50;  if  (Pe2f(f) >= 1),  PbS(f)=Pe2f(f)-PbR ; 
  PbE(f)= Pe2f(f) - 2 ; else  PbS(f)=  Q(f) - 156;  PbE(f)= Q(f)- 56;  end;
 elseif ( Afilemit == 215),PbR=50 ;  if  (Pe2f(f) >= 1), PbS(f)= Pe2f(f)-PbR;
PbE(f)= Pe2f(f) ; else  PbS(f)=  Q(f) - 156;  PbE(f)= Q(f)- 56;  end;
 if  isequal(Ca(f) , 'N' ) ,  PbS(f)= Pe2f(f) -27 ;   PbE(f)= Pe2f(f);  end; 
elseif ( Afilemit == 219),PbR=50 ;  if  (Pe2f(f) >= 1), PbS(f)= Pe2f(f)- PbR;
PbE(f)= Pe2f(f) ; else  PbS(f)=  Q(f) - 156;  PbE(f)= Q(f)- 56;  end; 
elseif ( Afilemit  == 220),if  (Pe2f(f) >= 1), PbS(f)= Pe2f(f) -PbR;
PbE(f)= Pe2f(f)-1  ;   else  PbS(f)=  Q(f) - 156;  PbE(f)= Q(f)- 56;  end;
      if  isequal(Ca(f) , 'N' ) ,  PbS(f)= Pe2f(f) -35 ;   PbE(f)= Pe2f(f);  end; 
 elseif (Afilemit == 221),  if  (Pe2f(f)>=1), PbS(f)= Pe2f(f)-PbR;    
 PbE(f)=Pe2f(f);    else  PbS(f)=  Q(f) - 156;  PbE(f)= Q(f)- 56;  end;   
elseif ( Afilemit==222 ), PbR =45 ; if  (Pe2f(f) >= 1), PbS(f)= Pe2f(f)-PbR ; 
PbE(f)= Pe2f(f) ;  else  PbS(f)=  Q(f) - 56;  PbE(f)= Q(f)- 6;  end;
      if  isequal(Ca(f) , 'J' ) ,  PbS(f)= Pe2f(f) - 15 ;   PbE(f)= Pe2f(f);  end; 
     if  isequal(Ca(f) , 'j' ) ,  PbS(f)= Pe2f(f) - 15 ;   PbE(f)= Pe2f(f);  end;    
elseif (Afilemit == 223), if  (Pe2f(f)>=1), PbS(f) =Pe2f(f)-PbR;
 PbE(f)=Pe2f(f)-2 ; else  PbS(f)=  Q(f) - 156;  PbE(f)= Q(f)- 56; 
 end;  if  isequal( Ca(f) , 'V') , PbS(f)= Pe2f(f) - 20 ;  PbE(f)= Pe2f(f);   end;
    if  isequal(  Ca(f) , 'A' ) ,PbS(f)= Pe2f(f) - 14 ;  PbE(f)= Pe2f(f) ;   end;
    if  isequal(  Ca(f) , 'a' ) , PbS(f)= Pe2f(f) - 14 ;  PbE(f)= Pe2f(f);end;
 % if  isequal(Ca(f) , 'N' ) , PbS(f)= Pe2f(f) - 4 ;  PbE(f)= Pe2f(f) ;     end;
elseif (Afilemit == 228),PbR = 50; if  (Pe2f(f)>= 1), PbS(f) = Pe2f(f)-PbR;
PbE(f)= Pe2f(f)-2 ;  else  PbS(f)=  Q(f) - 156;  PbE(f)= Q(f)- 56;  end; 
 elseif (Afilemit==230),PbR =48; if  (Pe2f(f) >=1), PbS(f)= Pe2f(f)-PbR; 
PbE(f)= Pe2f(f);  else  PbS(f)=  Q(f) - 156;  PbE(f)= Q(f)- 56; end;   
  if 0, if  isequal(  Ca(f) , 'A' ) ,PbS(f)= Pe2f(f)- 4;  PbE(f)= Pe2f(f);   end;
   if  isequal(  Ca(f) , 'a' ) ,PbS(f)= Pe2f(f) - 4 ;  PbE(f)= Pe2f(f) ;   end;
  if  isequal(  Ca(f) , 'S' ) , PbS(f) = Pe2f(f) -10 ;  PbE(f)= Pe2f(f) ;   end;
end;  elseif (Afilemit ==231),if  (Pe2f(f) >=1), PbS(f)=Pe2f(f)- PbR; 
PbE(f)= Pe2f(f); else  PbS(f)=  Q(f) - 156;  PbE(f)= Q(f)- 56;  end;   
  if 0, if  isequal(  Ca(f) , 'A' ) ,PbS(f)= Pe2f(f) - 4 ;  PbE(f)= Pe2f(f) ;   end;
   if  isequal(  Ca(f) , 'a' ) ,PbS(f)= Pe2f(f) - 4 ;  PbE(f)= Pe2f(f)   ;   end;
  if  isequal(  Ca(f) , 'S' ) , PbS(f) = Pe2f(f) - 10 ;  PbE(f)= Pe2f(f) ;     end;
end; elseif (Afilemit ==232),PbR = 50; if  (Pe2f(f) >=1), PbS(f)=Pe2f(f)-PbR; 
PbE(f)= Pe2f(f);  else  PbS(f)=  Q(f) - 156;  PbE(f)= Q(f)- 56;  end;    
    elseif ( Afilemit  == 233), if  (Pe2f(f) >= 1), PbS(f)= Pe2f(f)-PbR; 
  PbE(f)= Pe2f(f) ;   else   PbS(f)= Q(f) -100 ;  PbE(f)= Q(f)+2  ;
end; if  isequal( Ca(f) , 'V' ),PbS(f)= Pe2f(f) - 25;  PbE(f)= Pe2f(f); end;
 if  isequal(  Ca(f) , 'A' ) ,PbS(f)= Pe2f(f) -24 ;  PbE(f)= Pe2f(f)   ;   end;
elseif ( Afilemit  == 234),  if  (Pe2f(f) >= 1), PbS(f)= Pe2f(f) -PbR;  
PbE(f)= Pe2f(f); else   PbS(f)= Q(f) - 170 ;  PbE(f)= Q(f) - 32 ; end;  
else  if (Pe2f(f)>=1), PbR= 40;PbS(f)= Pe2f(f) - PbR; PbE(f)=Pe2f(f) ;
  else  PbS(f)=  Q(f) - 156;  PbE(f)= Q(f)- 56;    end;  
 end;  PbRange2(f) = PbE(f)-PbS(f);
 first_max = xbl1(Ra(f)) -xbl1(Q(f)); maxi = ((first_max - maxi)/4)+maxi; 
 pbslope_thresh= (thresh_pb/16)* maxi; pb_thresh=pb_param*pbslope_thresh;
  end;  for i = PbS(f) :  PbE(f)        
if PbS(f) <=0,  PbS(f)=1; end;  if i <=0,  i=1; end;
 if  (abs(slope(i,xbl1))> pb_thresh)&&(abs(slope(i+1,xbl1))>pb_thresh) ...
%&&(abs(slope(i+2,xbl1))> pb_thresh)% && (xbl1(i)>= 0)&&(xbl1(i-1)<=0)
  Pb1(f,j) = i ;  j =j+1; break ; end; 
  end; if ( j-1 >0), if (j-1) >0, Pb2(f)  = Pb1(f,j-1);  end;
 else   Pb2(f) = Pe2f( f);  zP = zP +1;   
% if (j-1>0) & exist('Pb1(f,j-1)','var')   Pb2(f)= Pb1(f,j-1); end;
fprintf(' 1620 f=%d Pb2(f) %d  Pe2f(f)  %d \n', f, Pb2(f)  , Pe2f(f) ) ;       
end;  if 1, PbSPb2(f)= Pb2(f)-PbS(f) ;  
 PbSPe2f(f)= Pe2f(f)-PbS(f);  if PbSPb2(f)< 1, PbSPb2(f) =1;  end; 
if ( f>=1)&&(Pb2(f)<VPon), wpbs(f)= PbS(f)-PbRange2(f)  
 if (S(f))&&(~isempty(S1)&&S(f)<DISPe),text(S(f),xbl1(S(f)),'S','color','r','FontSize',15);
%text(Ra(i),xbl1(Ra(i)+2 ), [int2str(i) string19{i} ] , 'color', 'r','FontSize', 16 );
 text( RR(f)+2, xbl1(RR(f)+2), 'R' ,'color', 'r' ,'FontSize', fonts18 ) ;     
end;  rectangle('Position', [ PbS(f), xbl1(PbS(f)), PbSPb2(f) , 0.5 ]  ,  ...
 'EdgeColor', 'r'  , 'LineWidth', 2);  filsig2 = [ 'color ', 'r' , ' FontSize ',  '18' ] ; 
if 10, text(PbS(f), xbl1( PbS(f))+ 0.815, ['Ponset'] , 'color', 'r' , 'FontSize', 18) ;
    
else text(Pb2(f), xbl1( Pb2(f))-0.2, [ '\uparrowPonset' ] ,'color', 'r' , 'FontSize', 16) ; 
end; text( PbS(f),xbl1( PbS(f))+  0.715 , 'Search',  'FontSize', 16,'color', 'r' ) ; 
text( PbS(f) , xbl1( PbS(f)) + 0.61 , 'range' ,  'FontSize' , 16  , 'color', 'r'  );
text( PbS(f) ,  xbl1( PbS(f)) + 0.55 , '\rightarrow',  'FontSize', 18 , 'color', 'r' );
text( Pe2f(f) , xbl1( Pe2f(f))-0.2, [ '\uparrowPoffset' ] , ...
 'color', 'r' , 'FontSize' , fonts18);  
fprintf('1638 f=%d Pb2%d %d Q%dR(f) %d\n', f, Pb2(f), Pe2f(f),Q(f),Ra(f));
if 0, [axx axy] = ginput(2) ; [ arrowx, arrowy] = dsxy2figxy( gca, axx, axy );
har= annotation(  'textarrow',  arrowx ,arrowy); [   'Ponset' int2str(Pb2(f)) ]
content=sprintf('(%4.2f,%4.2f)', axx(2),axy(2));set( har,'String',content,'Fontsize',18);
annotation( 'rectangle', [ PbS(f)-PbRange2(f), xbl1( PbS(f)), PbRange2(f),1 ])
end; if 0,rectangle('Position',[PbS(f)-PbRange2(f), xbl1(PbS(f)), PbRange2(f),1], ...
 'EdgeColor', 'y', 'LineWidth', 2 ) ;  end;  GPRLN(f) =PbS(f)-PbRange2(f) ;
if PbS(f)-PbRange2(f)<1,zpb(f)= 1;else zpb(f)= PbS(f)-PbRange2(f) ; end;
if (Pb2(f)-PbRange2(f)<1), zpb2(f)=1; else zpb2(f)= Pb2(f)-PbRange2(f); end;
end; end; end;  if 1, [ zlPb2 ]= find( Pb2<= 0 )
[ PbRange2max  PbRange2maxI ] =max( PbRange2 ) ; 
[ PbSPb2max PbSPb2maxI ] =max( PbSPb2 ) ;
[ PbSPe2fmax  PbSPe2fmaxI ] =max( PbSPe2f ) ; 
[ PbRange2min   PbRange2minI ]  = min( PbRange2 ) ; 
[ PbSPb2min PbSPb2minI ]=min( PbSPb2); [ zlPw ] = find((Pe2f-Pb2)<=0)
zlPwL= length( zlPw) ;  zlPbRange2= find(PbRange2<= 0); zlPb2 
 fprintf(' 1660  zlPwL =%d \n', zlPwL);  [zPw ] = find((Pe2f - Pb2)==0 ) 
 zlPwRa= Ra(zlPw )' ; zlPwCm1= '' ; if Afilemit == 222, ta  = ''  ; end;
for i = 1:  length( zlPw),   [  IR(i)  ] =find ( RO == Ra(zlPw(i))  ) ;   
if ~isempty(IR(i))  zlPwCm1(i)=C(IR(i)) ; zlPwCm1C(i)= char(zlPwCm1(i) ) ; 
end; end; cnptoqc= [  'Pon' Afilemits ] ; antyp=[]; 
hapon = get( gcf , 'CurrentAxes' ); % set( gcf , 'InvertHardCopy' , 'off' ) ; 
clear cnptoc  cnptxc cnptxoc Pb1;clear  filsig1  fmt1; atyp=[];  
FIGFILE= [ cnptoqc dates B   '.png']; 
wFI = [  PbSPb2max   PbSPe2fmax  pb_thresh   pbslope_thresh ];
FILEhPon = fullfile(PATFIG, FIGFILE);if DBUG, dbstop 1671;
    saveas( hPon, [ FILEhPon  ]);
end;end; end; if 1, if 10, hton=figure('NAME', ['1680Ton' Afilemits], ...
 'Position', [  20 40 screen(3)-60 screen(4)-160]);
h=gcf ;set(h,'WindowStyle','docked') ;  
 if ( Afilemit  ==114 ), Q(2) = Q(2)+(7);DISTb=720; start =1; startb= 1;   
 elseif ( Afilemit ==101 ),  DISTb=720; start =1 ;  startb= 1;DISTb=520; 
elseif ( Afilemit  ==202  ), start = 1; startb= S(1)-100;DISTb=Q(3);  
 elseif ( Afilemit  ==232  ), start = 1; startb= S(1)-100;DISTb=Q(3);     
 else DISTb=720; start = 1 ;  startb= 1; 
 end;  DRa = find(  Ra >= start & Ra<= DISTb ); DTb=Ra(DRa); 
 plot(( startb : DISTb ) ,  xbl1( startb : DISTb ) , 'b'  , 'LineWidth', 2 );
 minxbl1740 =min(xbl1(startb:DISTb )) ;
 maxxbf2=  max( xbl1(startb:DISTb) ) ;  xlim([startb   DISTb]) ;
if  fix((DISTb+startb+1)/2)>startb &&fix((DISTb+startb+1)/2)<DISTb
 uROI =[  startb  fix((DISTb+startb+1)/2)   DISTb ] ; 
else    uROI =[  startb:  fix((DISTb-startb+1)/2): DISTb ] ; 
 end;   set(gca,  'Units','normalized', 'FontUnits', 'points', ...  
'FontWeight', 'normal', 'FontSize', 17, 'FontName', 'Times');    
minxbf1p=min(xbl1(startb:DISTb ))-0.2;maxxbf1p=max(xbl1(startb:DISTb))+0.1;
line( [ startb, DISTb] , [ 0,0 ] , 'Marker' ,   '.'  , 'Color', 'r');
 titleTon =[ 'Tonset Search range from S,  Record  ', Afilemits ];   
title( [ titleTon ]  , 'color', 'r' ,  'FontSize' , 17);
 ampl =[  minxbf1p: (maxxbf1p- minxbf1p)/2  : maxxbf1p ] ;
 set(gca,  'Units', 'normalized',  'FontUnits',  'points',   ... 
 'FontWeight',  'normal', 'FontSize', 17 , 'FontName', 'Times' ); 
for i= 1: length( ampl ) [ cs935{ i,1}, ccTBE2] = sprintf('%5.2f',ampl(i))  ;
end;  set(gca,    'YTick', ampl ); set(gca, 'YTickLabel', cs935);  
 set( gca,'XTick', uROI ); set( gca,'XTickLabel', uROI ) ;   
 ylim( [ minxbf1p maxxbf1p]);   grid off ;     ROM60= 10; 
 xlabel(' Time in samples' , 'FontSize' , fonts18  , 'Color', 'b'  );  
 ylabel('Amplitude, (mV)', 'FontSize', 18, 'Color', 'b');
 xlim( [startb-1  DISTb ]) ; RL= ( length(Ra)); zTB1F =[]; RaL = RL;
 % if Afilemit == 201 , RaL = RL-1;   else RaL = RL; end;  
 maxi = slomax250; thresh_param=2; pt_param = 0.2;
  f  = 1 ;  first_max = xbl1(Ra(f))- xbl1(Q(f));  filter_paramTb = 4;
  maxi =  maxi  + ( ( first_max - maxi ) / filter_paramTb) ;
  slope_threshTb=(thresh_param/16 )*maxi; 
  pt_threshTb = pt_param*slope_threshTb;  htona =figure(hton) ;
end; for f  = 1 :  RaL   j=1;  if 1,   %Tb=[]; Tb1f
if  f<length(Ra), RRNdur3(f)= Ra(f+1)-Ra(f);  else  RRNdur3(f)= NS-Ra(f); 
end;     Ritmin3(f) =  fix( 0.111 * RRNdur3(f));
 Ritmax3(f) =  fix( 0.583 * RRNdur3(f));  TBE2(f) = Ra(f) + Ritmin3(f)-10 ;
if  ( Afilemit == 100), TBs2(f) = S(f)+ 20   ; TBE2(f) = S(f) + 70 ;      
elseif  ( Afilemit  == 101 ), TBs2(f) = S(f) + 10 ; TBE2(f) = S(f) + 50 ;  
 
elseif  ( Afilemit  ==105 ), TBs2(f) = S(f) + 10 ; TBE2(f) = S(f) + 50 ;   
elseif ( Afilemit==108),  TBs2(f) = S(f) +10; TBE2(f)= S(f) +50; 
zpfilter_paramTb =16; 
elseif  ( Afilemit  ==  111 ),   TBs2(f) = S(f) + 10 ; TBE2(f) = S(f) + 50 ;
     elseif  ( Afilemit  ==  113 ), TBs2(f) = S(f) + 10 ;  TBE2(f) = S(f) + 60 ;
  elseif  ( Afilemit  ==  115 ),   TBs2(f) = S(f) +5; TBE2(f) = S(f) +30 ;    
     elseif  ( Afilemit  ==  116 ),   TBs2(f) = S(f) + 10 ; TBE2(f) = S(f) + 50 ;
     elseif  ( Afilemit ==  118 ),  TBs2(f) = S(f) + 10 ; TBE2(f) = S(f) + 50 ;
        elseif  ( Afilemit ==  119 ), TBs2(f) = S(f) + 10 ; TBE2(f) = S(f) + 50 ;
 elseif  ( Afilemit  ==  123 ),  TBs2(f) = S(f) + 10 ; TBE2(f) = S(f) + 50 ;
     elseif  ( Afilemit  ==  124 ), TBs2(f) = S(f) + 30 ; TBE2(f) = S(f) + 50 ;
     elseif  ( Afilemit  ==  201 ),TBs2(f) = S(f) + 30 ;  TBE2(f) = S(f)+ 50  ;
     elseif  ( Afilemit  ==  202 ),      TBs2(f) = S(f) + 15 ; TBE2(f) = S(f) + 50 ;
     elseif  ( Afilemit  ==  203 ) , TBs2(f) = S(f) + 10 ; TBE2(f) = S(f) + 50 ;
     elseif  ( Afilemit  ==  205 ) , TBs2(f) = S(f) + 4 ; TBE2(f) = S(f) + 50 ;
         
  elseif  ( Afilemit  ==  207 ), TBs2(f) = S(f) + 2 ; TBE2(f) = S(f) + 50 ;  
  if  isequal( Ca(f ) , 'E' ) ,  TBs2(f) = S(f) + 0 ; TBE2(f) = S(f) + 50 ; end; 
elseif  ( Afilemit  ==  208 ) ,  TBs2(f) = S(f) + 1 ; TBE2(f) = S(f) + 50 ;
    
 if  isequal( Ca(f ) , 'V' ) ,  TBs2(f) = S(f) + 0 ; TBE2(f) = S(f) + 50 ; end; 

elseif  ( Afilemit  ==  209 ) , TBs2(f) = S(f) + 10 ; TBE2(f) = S(f) + 50 ;
     elseif  ( Afilemit  ==  213 ) , TBs2(f) = S(f) + 10 ; TBE2(f) = S(f) + 50 ;
       elseif  ( Afilemit  ==  214 ) , TBs2(f) = S(f) +5 ; TBE2(f) = S(f) +35 ;
           TBs2(f) = S(f) + 10 ; TBE2(f) = S(f) + 50 ;       
elseif  ( Afilemit  ==  215 ) , TBs2(f) = S(f) + 10 ; TBE2(f) = S(f) + 50 ;   
    
 elseif  ( Afilemit  ==  221 ),  TBs2(f) = S(f) + 10 ; TBE2(f) = S(f) + 50 ;
elseif ( Afilemit==223), TBs2(f)= S(f) +10; TBE2(f) = S(f) +12; 
 if (f==246), TBE2(f) = S(f) +12;   end;
   elseif  ( Afilemit  == 228 ), TBs2(f) = S(f)+4 ; TBE2(f) = S(f) + 70 ;        
     elseif  ( Afilemit  ==  230 ),  TBs2(f) = S(f) + 10 ; TBE2(f) = S(f) + 50 ;
 elseif  ( Afilemit  ==  231 ),  TBs2(f) = S(f) +4 ; TBE2(f) = S(f) + 40 ;          
elseif  ( Afilemit  == 232 ), TBs2(f) = S(f)+20 ; TBE2(f) = S(f) +40 ;
         
elseif  ( Afilemit  ==  233 ), TBs2(f) = S(f) +20 ; TBE2(f) = S(f) +40 ;
if f+1<=length(Ca)& isequal(Ca(f+1),'a'),TBs2(f)=S(f)+30;TBE2(f)=S(f)+70;end;
 
elseif ( Afilemit  ==  234 ), TBs2(f) = S(f) +10 ; TBE2(f) = S(f) +40 ;
    %zpfilter_paramTb= 16 ;
else    TBs2(f) = S(f) +5 ; TBE2(f) = S(f) +35 ;
end; first_max = xbl1(Ra(f))- xbl1(Q(f));
  maxi = maxi  + ( ( first_max - maxi ) / filter_paramTb) ;
  slope_threshTb=(thresh_param/16)*maxi;  
  pt_threshTb = pt_param*slope_threshTb; Tb3range3(f) =TBE2(f)- S(f); 
end;  for i = TBs2( f ) : TBE2(f) 
    if i >= NS-5 , break; end;
 if  (abs(slope(i,xbl1))>pt_threshTb) &&(abs(slope(i-1,xbl1))>pt_threshTb) ...
 %&& (slope(i+2)> pt_thresh),
%if (f<length(Ra)) &&(i+1>= qn(f+1,1)-50), %f=f+1; j=1;%continue;end;
Tb(f,j) =i-1;  j=j+1; break;  
end;end;   if (j-1>0), Tb1f(f)=Tb(f, j-1); 
 else Tb1f( f ) = S(f );  zTB1F =[ zTB1F f ];  
end; if Tb1f(f)-S(f)+0 <= 0,wTbS(f)=10;  else wTbS(f)=Tb1f(f)-S(f)+0; 
end; TBs2S(f)  =TBs2(f) - S(f) ;
if  (Tb1f(f)<= DISTb), 
if Tb1f(f )< DISTb, if DBUG dbstop 1775; 
 end;text( Tb1f(f), xbl1(Tb1f( f ))-0.1,[ '\uparrow' 'Tonset'] ,'color','r' ,'FontSize',17); 
fprintf('1770  f= %dPb2=%dPe2f=%dQ(f)=%d Ra=%d S=%dTb1f=%d\n', ...
 f , Pb2(f), Pe2f(f), Q(f) ,Ra(f), S(f), Tb1f( f));zlensort =[ int2str(f)  'Tonset' 'search' ]; 
fprintf(' 1778  Tb1f= %dPb2( f+1)= %d  Pe2f( f+1)=%d Ra(f+1)%d  \n', ...
 Tb1f( f ) , Pb2( f+1), Pe2f( f+1) , Ra(f+1)   ); %   
xtb(f,:) = [ Tb1f(f), xbl1(Tb1f(f)),Tb1f(f)-S(f)+2 , xbl1(Tb1f(f))+0.5 ] 
rectangle( 'Position', [S(f), xbl1(Tb1f(f)), wTbS(f) , 0.5 ]   ...
, 'EdgeColor', cr  , 'LineWidth', 2  ) ; 
text( S(f) ,   0.65 ,  'search' ,  'FontSize', 18 , 'color', cr   ); 
 text( S(f) ,   0.58 ,  'range' ,  'FontSize', 18 , 'color', cr  );
 text( S(f),  0.5 , '\rightarrow',  'FontSize', 18  ,'color', cr ) ;
 if  Afilemit < 400,  string19{f}=[ 'R' ]; 
else   string19{f }= [ (  Ca(f)  )  num2str( Ra(f)) ] ;  
end; text( Ra(f)+2,  xbl1( Ra(f)+2 ) , [  string19{f} ]  ,'color', 'r' , 'FontSize', 18 );
if (RR(f)<= DN), text(S(f)-2 , xbl1(S(f)-2)-0.04,[ 'S' ] , 'color', 'r' , 'FontSize',16 );
  text(Q(f)-1,xbl1(Q(f)-1)-0.01, 'Q' ,'color', 'r' ,'FontSize', 16 );  
end; end; end; end;  if 1,  zTb1f= find (Tb1f ==0);   
[ Tb3range3max  Tb3range3maxI ]=  max( Tb3range3) ;  uC
[ wTbSmax  wTbSmaxI ]= max( wTbS); [TBs2Smax TBs2SmaxI ]=max(TBs2S);
[ Tb3range3min ] =  min(Tb3range3 ); [ wTbSmin wTbSminI] = min(wTbS) ;
wJM =  [TBs2Smax wTbSmax  slope_threshTb  pt_threshTb ];
cnptoqc= [ 'Ton' Afilemits ] ; zlTb1f = find(Tb1f<=0 ); CazlTb1f = Ca(zlTb1f)
  if (Afilemit == 233), RL= ( length(Ra) - 0); else RL= length(Ra); end;
  AIR1180= find( xbl1(Ra)<= 0 ); AIR1180ca=Ca(AIR1180 ); Te1= [];  ; 
  dates= [ cnptoqc d(1:2), d(4:6), d(10:11), d(13:14), d(16:17) B ] ;  
  FIGFILE= [ cnptoqc  B dates , '.png' ] ;  
FILEhton= fullfile( PATFIG, FIGFILE) ; if  DBUG, dbstop 1802 ; 
saveas( hton , [ FILEhton ] ) ; 
end;  lead=1;  leads=int2str( lead) ;   ROM2 = [  ] ; 
hTOFF1 =figure( 'NAME', [Afilemits '1803 T offset ' ...
uCa 'L'  leads ] , 'Position',  [ 10  20 screen(3)-80 screen(4)-120 ] ) ; 
h =gcf ;  set(h,'WindowStyle','docked') ;  TeR=80;
end; end; end; if 1, if 1, [ Tef1,TeS3Tef1, wNQ ] =CallToffset170104 ...
 ( xbl1 , Afilemit ,Ca, Ra, NS, Q, S, Pb2, Pe2f,Tb1f,B , PATFIG,lead )  ;
 GP2nan= [ 'Toffsetg' Afilemits  B  dates '.png'];  lead=2; leads=int2str( lead);
 %hTOFF1g =get( gcf , 'CurrentAxes' );  set( gcf ,'InvertHardCopy', 'off' %) ;
  FIGUFITe = fullfile( PATFIG, GP2nan); if DBUG, dbstop 1812 ; 
saveas( hTOFF1 , [ FIGUFITe ] ); 
  end;  hTOFF2=figure( 'NAME', ['1790Toffset'   ...
uCa ' L' leads], 'Position',  [ 10  20  screen(3)/1-40 screen(4)/1-100] ) ;    
  [Tef2,TeS3Tef2,wNQ2] =CallToffset170102 ...
( xbl2 , Afilemit ,Ca, Ra, NS, Q, S, Pb2, Pe2f,Tb1f,B , PATFIG,lead )  ;
 lead=3;leads=int2str( lead);hTOFF3=figure('NAME', ['1780Toffset'  ...
uCa 'L' leads], 'Position',  [ 10  20  screen(3)/1-40 screen(4)/1-100]  ) ; 
[Tef3,TeS3Tef3,wNQ3  ] =CallToffset170102 ...
( xbl3 , Afilemit ,Ca, Ra, NS, Q, S, Pb2, Pe2f, Tb1f,B , PATFIG ,lead  ) ;
end; if 10, wpbe= [ PeSQmax  Pe2fQmax  PeRange2max pept_thresh];
wFI =  [  PbSPb2max   PbSPe2fmax  pb_thresh   maxi ];
wJM = [ TBs2Smax wTbSmax  slope_threshTb  pt_threshTb ]; 
w = [ wpbe   wFI   wJM     wNQ  ] ;  
%waaaj=[pefilter_param pethresh_param  pb_param pbslope_thresh pt_param]; 
[RRdurmin RRdurminI ]=min(RRdur);[RRdurmax RRmaxI ]= max(RRdur);
 Pb2O = Pb2;   Pe2fO =  Pe2f ;  Tb1fO  =  Tb1f ; 
end;  if  exist('Tef1', 'var'), Tef1O =Tef1; zlTef1=find (Tef1<= 0)
 lTb1f =length(Tb1f); lTef1 = length(Tef1);  lTw = min(lTb1f, lTef1); 
 [ zlTw  ]= find( ( Tef1(1:lTw)- Tb1f(1:lTw))<=0);  zlTwlen =length(zlTw)
 fprintf('1805; zlPwlen=%dzlTwlen=%d\n', zlPwL, zlTwlen); zlTwca=Ca(zlTw)'
zlPwS= num2str( length( zlPw) ) ; zlTwS= num2str( length(zlTw) );  
lPb2 = length( Pb2) ; len= min( lPb2, lTef1 ) 
 Annconpto= [ zlPwS 'zlPw'  zlTwS  'zlTw'] ;  
end; if exist('ANNT', 'var'), BCM = 'M' ;
 Pb2M =ANNT.Ponset;  Pb2MnanN= find (~isnan (Pb2M));     
 clear cnp cnpc cnptc ANN ANNCONP  CNB ANNCONP  CNB;
Pe2fM = ANNT.Poffset ;  Pe2fMnanN=  find( ~isnan(Pe2fM) )  ;
Tb1fM = ANNT.Tonset ; Tb1fMnanN =  find( ~isnan(Tb1fM ) ) ;
Tef1M = ANNT.Toffset ; Tef1MnanN =  find( ~isnan(Tef1M )) ;
Pb2Mnan =find(isnan(Pb2M)); Tef1Mnan =find( isnan(Tef1M )) ; 
 Pe2fMnan=  find( isnan(Pe2fM));Tb1fMnan =find( isnan(Tb1fM )) ;
 MoodyfailP =100* length( Pb2Mnan)/RL ;%TPM=ANNT.T;
  MoodyfailT=100* length( Tef1Mnan) /RL;%TPMnanN=find(~isnan(TPM));
ACAPCIN=intersect(ACAPC,Pb2Mnan);ACPVCIN=intersect(ACPVC,Pb2Mnan); 
A3=intersect( Tef1Mnan, Pb2Mnan); 
end; if exist('ANNT', 'var') &&~Bthesis, if ( Afilemit ~=233 ...
  &&( Afilemit ~=108)&&( Afilemit ~=228) ... 
&&( Afilemit ~=  205 ) && ( Afilemit ~=  232 ) )  %|| 1  
  Pb2(Pb2MnanN)   =  Pb2M(Pb2MnanN); BCM = 'M' ;
 Pe2f(Pe2fMnanN) = Pe2fM(Pe2fMnanN) ;   BcM  =  'MP' ;
 end; if  (Afilemit ~= 233 && ( Afilemit ~=108) &&( Afilemit ~= 228)...
 &&( Afilemit ~=  205 ) &&( Afilemit ~=  232 ) ),
Tb1f (Tb1fMnanN) = Tb1fM(Tb1fMnanN) ; BcM= [ BcM  'MT']; 
Tef1(Tef1MnanN) = Tef1M(Tef1MnanN) ;% [left bottom width height] 
end; 
end;end;end; if 1, hLimits=figure( 'Name', ['1860MoodyPT'  Annconpto], ...
 'Position',[10 -4 screen(3)-50 screen(4)-60]); h=gcf;set(h,'WindowStyle','docked'); 
if  (Afilemit == 205),   U1=Q(1)-100; V1= Q(3); nan=0.2;
elseif  (Afilemit == 232), U1 =   Q(1)-100; V1= Q(3);nan=0.2;  
 else  U1=Pb2(1)-50;; V1 =Tef1( 3 )+50; nan=0.2;
 end; if 1, N=2; m=0; m=m+1 ; hsub1=subplot( N , 1, m ) ; 
 if  U1<1, U1=1; end;  plot(U1 : V1 , xbl1( U1 : V1) , cyan , 'LineWidth', 2 );   
  [DBT  ]= find(Ra>= start & Ra<= start+V1-1);  DRT=Ra(DBT); 
A3= U1: floor((V1- U1-1)/2) :V1 ;   [ minxbf1 ] =  min( xbl1( U1: V1) )-0.1 ; 
 [ maxxbf1 ]=max( xbl1( U1 : V1 ))+0.1;  RA3= [ minxbf1 0  maxxbf1]; 
fy2=(minxbf1:((maxxbf1-minxbf1)/2):maxxbf1); 
set(gca,  'Units','normalized', 'YTick',  fy2 ...
 , 'XTick', A3 , 'FontUnits',  'points'  ...  %,'Position', [ .15 .2 .75 .7 ] ...
 , 'FontWeight',  'normal',   'FontSize', 20 , 'FontName', 'Times' ) ;     
if BL, line( [ U1,V1] , [0,0] , 'Marker' ,   '.'  , 'Color', 'r' ); 
elseif 1,  line([ U1, V1], [ 0,0 ] , 'Marker' , '.', 'Color', 'k' , 'LineWidth', 1);         
end; for i= 1: length( fy2)  [ ST1PT{ i,1} , ceTB2] = sprintf('%.2f',  fy2(i));end;   
 set(gca, 'YTickLabel',  ST1PT ) ; ylim([  minxbf1 maxxbf1] ); 
filter_param = 16;    thresh_param= 2;   pb_param = 0.1;xlim([U1-1  V1]) ;  
[maxi ,slomaxi]=max(slopes(3,250,xbl1));clear ANNOT CHANFIELD DM FM Der;   
 zlensort1170='Laguna''s ecgpuwave software, Record ';
title( [zlensort1170 Afilemits ],'color', 'r' ,'FontSize' ,18 ); 
if 0,  xlabel(' Time in samples' , 'FontSize' , fonts16  , 'Color', 'b' );  
  ylabel('Amplitude, mV'  ,   'FontSize', fonts16  , 'Color', 'b'  );
end;  [DBT  ]= find(Ra>= U1 & Ra<= V1);  DRT=Ra(DBT);  
 for k=1: length(DRT)      K2 = k ; f= k ; 
if Afilemit<400, string19{k }=[num2str((k))  (Ca(k)) num2str(Ra(k)) ]; 
else string19{k }=[ num2str(Ra(k)) ] ;
end; text(Ra(k)+2,  xbl1(Ra(k) +2) , [string19{k} Ca(k) ],'color','b' ,'FontSize',15);
 if exist('Pb2M', 'var')&&(~isnan(Pb2M(f))), %&& isequal( BCM, 'M' )  
 text(Pb2M(f),xbl1( Pb2M(f)),[  'Pb' ],'color',cb  ,'FontSize', 14);
elseif 0,  text( Pb2( f ) ,  xbl1( Pb2( f) ),  [ 'NANPb' ] , 'color', cr  ,'FontSize', 14 );
 end;   if  exist('Pe2fM', 'var') &&(~isnan(Pe2fM( f)))&&(~isempty(Pe2fM))
 text( Pe2fM( f ) , xbl1( Pe2fM( f) ),[  'Pe' ] , 'color', cb ,'FontSize', 14 ); 
 elseif 0, text( Pe2f(f) ,  xbl1( Pe2f(f) ),['NANPe'  ] , 'color', cr ,'FontSize',14); 
  end;  if  exist('Tb1fM', 'var') &&~isnan(Tb1fM(K2) ) &&(f<= length(Tb1fM))
  text(Tb1fM( K2) , xbl1( Tb1fM( K2) ),[  'Tb'   ] , 'color', 'b'  ,'FontSize', 14 );
elseif 0,text( Tb1fO(f) ,  xbl1( Tb1fO( f) ), [' '  'NANTb' ] , 'color', 'r'  ,'FontSize',14);
end; if  exist('Tef1M', 'var') && ( ~isnan (Tef1M( f ) ) )
  text( Tef1M( f ) , xbl1( Tef1M( f) ),[   'Te' ] , 'color', 'b'  ,'FontSize', 14  );
elseif 0,  text(Tef1O(K2), xbl1(Tef1O( K2)),['NANTe'] ,'color','r'  ,'FontSize',14 );
end; if 0, if (SM(f)>0) && (~isempty(SM)), text(SM(f), xbl1(SM(f)), 'SM' );end;  
end; end; if exist('ANNT', 'var'), %leghm.Title.Visible = 'on';
%[ leghm]=legend(Pb2MnanN, Tb1fMnanN );title(leghm,'Detection Failure rate' )
Pe2fMnanN=[ 'P wave:' num2str(MoodyfailP,'%4.1f') '%' ] ; 
Tef1MnanN=[ 'T wave:' num2str(MoodyfailT,'%4.1f') '%' ] ; 
Tb1fMnan = sprintf('%4.1f',MoodyfailT);Tb1fMnanN=[ 'T wave:' Tb1fMnan '%' ] ; 
 Pb2Mnan=  sprintf('%4.1f',MoodyfailP);Pb2MnanN=['P wave:' Pb2Mnan  '%'];
 [ leghm,oh,plh,ts]=legend(Pb2MnanN, Tb1fMnanN ); 
%  set(leghm, 'position', get(hsub1, 'position'));
v=get(leghm,'title');   set( v,'string' ,'Detection Failure' ,'FontSize', 14,'color',cr);
 if 0, newPosition = [ 0.7 0.6 0.2 0.2];   newUnits = 'normalized';
set(leghm,'Position', newPosition, 'Units', newUnits);pos = get(leghm,'position') ;
text(pos(1)+1.4,pos(2)+0.16,'My Legend Title') ;
% set(findall(leghm, 'string', 'my title'), 'fontweight', 'bold'); legend('boxon')
  end;%set(leghm , 'Position',  [ DRT(1) xbl1(DRT(1))-0.5 0.5 0.5]  );
  end;  end;  if N>1, m=m+1; subplot( N , 1, m ) ; 
 plot(U1 : V1, xbl1( U1 : V1) , cm , 'LineWidth', 2 );  
  A3= U1: floor((V1- U1-1)/2) :V1 ; % [ minxbf1 ] = min( xbl2( U1 : V1 )) ;
if (Afilemit==201),[minxbf1]=min(xbl1(U1:V1))-0.1;[maxxbf1]=max(xbl1(U1:V1))+0.1;
else minxbf1 =min( xbl1(U1 : V1))-0.1; [ maxxbf1 ]=max( xbl1(U1:V1))+0.1;
end; RA3=[ minxbf1 0 maxxbf1]; fy2=( minxbf1: ((maxxbf1-minxbf1)/2):maxxbf1) ;
set(gca, 'Units',  'normalized'  , 'FontUnits',  'points' ...
, 'FontWeight',  'normal',  'XTick', A3,'YTick', fy2, ... 
 'FontSize', 20 , 'FontName', 'Times') ; % hTitle = legendTitle(leghm,'your title string') 
if BL, line( [ U1,V1] , [0,0] , 'Marker' , '.'  , 'Color', 'r'); 
 elseif 1,  line([ U, V], [ 0,0 ] , 'Marker' , '.', 'Color', 'k' , 'LineWidth', 1);      
end; for i= 1: length(fy2)  [ ST1PT{i,1}, ceBE2] = sprintf('%.2f',fy2(i));end;
set(gca, 'YTickLabel',  ST1PT ) ; ylim([  minxbf1 maxxbf1] ); 
title(['Tan''s Slope algorithm for P and T waves, Record ' Afilemits]...
, 'color', 'r','FontSize' ,18 ); xlim([U1-1  V1 ]) ; 
for f=1 : length(DRT)   K2 = f ;
if 0, text(Ra(f)+2,xbl1(Ra(f)),[int2str(f) Ca(f) int2str(Ra(f))],'color',cb,'FontSize',14);
end; if (Pe2f(f)-Pb2(f))>1, text(Pb2(f), xbl1(Pb2(f)),[ 'Pb'] , 'color', 'b','FontSize',14);
text( Pe2f(f),xbl1( Pe2f( f)), [  'Pe' ], 'color', 'b', 'FontSize', 14  ); 
 end; if  (Tb1f(K2)), text(Tb1f(K2), xbl1(Tb1f(K2)),['Tb' ], 'color','b','FontSize',14); 
 end; if  (Tef1( K2) > 0)  &&  (Tef1( K2)<=V1 ),    
text(Tef1(K2),xbl1( Tef1( K2) ),[  'Te'] ,'color', 'b' ,'FontSize', fonts14 );
end; if 0,  text(Q(f ), xbl1(Q(f )), ['Q' ],'color', 'r' ,'FontSize', fonts16); 
 text(S(f ), xbl1(S(f )), 'S' , 'color', 'r' ,'FontSize',fonts14  ); 
 end; end; end; cnptoqc= ['MoodyTan' Afilemits ];  clear   xd1 ;
FIGFILE= [ cnptoqc  d(1:2), d(4:6), d(10:11), d(13:14), d(16:17) B, '.png'];
FIGUFILE =fullfile(PATFIG, FIGFILE); mnum=[]; if DBUG, dbstop 1940; 
     saveas( hLimits, [ FIGUFILE  ]);
end; end;  if 10, if 28, if 27, if 19, if 1, fprintf(' correction uC\n');                 
if (Afilemit == 101),AC101= [ 39 208 361 362 371 1286 ];  
 for i= 1: length(AC101) zsleft=  AC101(i) ;
Pe2f(zsleft)=Q(zsleft)-10; Pb2(zsleft)=Q(zsleft)-20;
Tb1f(zsleft) =  S(zsleft)+4;  Tef1(zsleft) =  S(zsleft)+14;
 end; end; if (Afilemit==103)  %AC= [  1268 ] ; 
 zsleft=  1268 ; Pe2f(zsleft) = Q(zsleft)-10;     Pb2(zsleft) =    Q(zsleft)-20;
 elseif (Afilemit==  105 ), AC = [     1821 ];    
elseif (Afilemit==  108 ), AC = [   282  700  ];    
  zsleft= 141 ;Tb1f(zsleft) =   Tb1fO(zsleft) ;      Tef1(zsleft) =  Tef1O(zsleft) ;
 zsleft= 224 ; Tb1f(zsleft) =   Tb1fO(zsleft) ;      Tef1(zsleft) =  Tef1O(zsleft) ;
zsleft= 243 ;     Tb1f(zsleft) =   Tb1fO(zsleft) ;  Tef1(zsleft) =  Tef1O(zsleft) ;
 zsleft= 1105;Tb1f(zsleft) =   Tb1fO(zsleft) ;      Tef1(zsleft) =  Tef1O(zsleft) ;
 zsleft= 1142 ;   Tef1(zsleft) =      Tef1O(zsleft) ;  
zsleft= 1150 ;Tb1f(zsleft) =   Tb1fO(zsleft) ;      Tef1(zsleft) =  Tef1O(zsleft) ;  
zsleft= 1159 ;Tb1f(zsleft) =   Tb1fO(zsleft) ;      Tef1(zsleft) =  Tef1O(zsleft) ;   
zsleft=   1148 ;   Pe2f(zsleft) =    Q(zsleft)-10;     Pb2(zsleft) =    Q(zsleft)-20 ;      
Tb1f(zsleft) =   S(zsleft)+24;         Tef1(zsleft) =   S(zsleft)+34; 
zsleft= 1589 ; Tb1f(zsleft) =   Tb1fO(zsleft) ;      Tef1(zsleft) =  Tef1O(zsleft) ;  
zsleft= 1606 ;     Tb1f(zsleft) =   Tb1fO(zsleft) ; Tef1(zsleft) =  Tef1O(zsleft) ;  
zsleft=    1652 ; Pe2f(zsleft) =    Q(zsleft)-10;     Pb2(zsleft)=Q(zsleft)-20; 
      Tb1f(zsleft) =   S(zsleft)+24;         Tef1(zsleft) =   S(zsleft)+34;   
zsleft=  1665 ;  Pe2f(zsleft) =    Q(zsleft)-10;     Pb2(zsleft) =    Q(zsleft)-20; 
      Tb1f(zsleft) =   S(zsleft)+24;         Tef1(zsleft) =   S(zsleft)+34;  
zsleft=  1747 ;  Pe2f(zsleft) =    Q(zsleft)-10;     Pb2(zsleft) =    Q(zsleft)-20; 
      Tb1f(zsleft) =   S(zsleft)+24;         Tef1(zsleft) =   S(zsleft)+34;   
elseif (Afilemit==114)  AC= [   ] ; 
 zsleft=  314 ; Pe2f(zsleft) = Q(zsleft)-10;Pb2(zsleft) = Q(zsleft)-20; 
zsleft=   737 ;  Pe2f(zsleft) =    Q(zsleft)-1;   Pb2(zsleft) = Q(zsleft)-4 ;
elseif (Afilemit== 115), AC = [ 18 43 47 52 66  64 146 153 222  226 320 325 ...
        ];    
 zsleft=  1446 ;  Pe2f(zsleft) =    Q(zsleft)-1;   Pb2(zsleft) =  Q(zsleft)-6; 
end; if (Afilemit==116 ) 
    AC= [ 128 204 207 273 347 350 352 382 461 131 386  ...
 681 723  737 753 762  846 517 547 575 551 561 639  484 2091 195 659 ...
 1258 1318 1321    1795 1538 1794 1796 1799 1843 1860 2085 2081 2121] ; 
zsleft=842;Pe2f(zsleft) =Q(zsleft)-1; Pb2(zsleft) = Q(zsleft)-3 ;
Tb1f(zsleft) =S(zsleft)+24; 
zsleft= 1845 ;  Pe2f(zsleft) =Q(zsleft)-1;     Pb2(zsleft) = Q(zsleft)-3 ;   
zsleft=1797; Pe2f(zsleft) =Pe2fO(zsleft);  Pb2(zsleft) =Pb2O(zsleft); 
 Tb1f(zsleft) =Tb1fO(zsleft) ;  
zsleft=1861 ; Pe2f(zsleft) = Pe2fO(zsleft);  Pb2(zsleft) = Pb2O(zsleft); 
  zsleft= 2060 ;  Pe2f(zsleft) =    Q(zsleft)-1;  Pb2(zsleft) = Q(zsleft)-3 ; 
 end; if (Afilemit==124)  AC= [ 916 925 933 ] ; 
 zsleft=  244 ; Pe2f(zsleft) = Q(zsleft)-10;Pb2(zsleft) = Q(zsleft)-20;     
 zsleft=  916 ; Pe2f(zsleft) = Q(zsleft)-10;Pb2(zsleft) = Q(zsleft)-20;  
  zsleft=  925 ; Pe2f(zsleft) = Q(zsleft)-10;Pb2(zsleft) = Q(zsleft)-20; 
   zsleft=  933 ; Pe2f(zsleft) = Q(zsleft)-10;Pb2(zsleft) = Q(zsleft)-20;  
 end; if (Afilemit==  205 ), AC = [24  4 5 9 16 20 21   31 47  988   457  459   ...
 804   2287  458 2430  1007   1245        2561  ];  
      zsleft=669 ;  Pe2f(zsleft) =    Q(zsleft)-1;   Pb2(zsleft) =  Q(zsleft)-6;      
 end; if (Afilemit==214),  zsleft=203;Pe2f(zsleft)=Q(zsleft)-1; Pb2(zsleft)=Q(zsleft)-6; 
      zsleft=  500 ;  Pe2f(zsleft) =    Q(zsleft)-1;   Pb2(zsleft) =  Q(zsleft)-6; 
 zsleft=  627 ;  Pe2f(zsleft) =  Q(zsleft)-1;   Pb2(zsleft) =  Q(zsleft)-6; 
  zsleft=  773 ;  Pe2f(zsleft) =    Q(zsleft)-1;   Pb2(zsleft) =  Q(zsleft)-6; 
 zsleft=  1667 ; Pe2f(zsleft) = Q(zsleft)-10;Pb2(zsleft) = Q(zsleft)-20;
AC= [11 331 566 573 577 604  20 446  1453 476 1343 1356  1463 1462 1481]; 
 end; if (Afilemit==  215 ), AC = [  5  289 775];    
  zsleft=5 ; Pe2f(zsleft) =  Q(zsleft)-1;   Pb2(zsleft) =  Q(zsleft)-6;   
   elseif (Afilemit== 221 ),AC = [  7 304 8  443 2013  1366   1269   646 12  978 ...
        798   2173  18  2285  2073 20 2240  39 758  59 1276  104  1935   2030 ...
 116         119         142         154         156         160   ...
 ];  
end; if (Afilemit==  223 ), AC = [  2169   ];    
 zsleft=2166;  Pe2f(zsleft) = Q(zsleft)-1; Pb2(zsleft) = Q(zsleft)-6;   
 end; if (Afilemit== 228), AC = [2,27,112,124,  134,191, 361, 543, 596,...
 594,    599,757, 922,  1470, 1472, 1590 ,  1481, 1842, ...
1 ,    4 ,1412, 1468,   18,26   , 33  ,  63  , 199 ,  204 ,  259 ,  345,   922 ,...
 425 , 431 , 435 ,1489,1416 ,1835] ;    
 elseif (Afilemit== 230 ),AC = [  2245  ];  
elseif (Afilemit==231), AC =[ 444 458 782  421 80 1123 860 421 466  1143 ...
381  460   754   791  1137 115  406     449  407   442   758   808  807   ...
792   1138  1149 7  799   1156  1154  1121 1125 ];   
 elseif (Afilemit== 232 ), AC = [  47];
end;end;  if 10, if (Afilemit~=  232 ) for i= 1: length(AC ) zsleft=AC(i) ;
Pe2f(zsleft)=Q(zsleft)-10; Pb2(zsleft)=Q(zsleft)-20;
Tb1f(zsleft) =   S(zsleft)+4;  Tef1(zsleft) =  S(zsleft)+14;
% if (Afilemit ==111),xbl2=xbl2-0.03;  end;      
 end; end;   if 1, Asubtyp= [ 'Length' Afilemits  ]; 
xu=' R position'; clear annAmp QTC ANNT ; TIM= [ ''];xdfc = [ ] ; 
clear A1ST  ANNT   PbE     ANNOT   AMP*  SUBTYPEFIELD  POS* ; 
 if exist( 'hTOFF2', 'var' ),  hTOFF2s= close(hTOFF2); clear hTOFF2; end; 
 if exist( 'hTOFF3', 'var' ),  hTOFF3s=close( hTOFF3);clear hTOFF3;end; 
 [ zPwM]= find((Pe2f-Pb2 )< 0); [ zlTwM]=find((Tef1(1:lTw) -Tb1f(1:lTw))<=0)
 zlen1170=Tef1(1:len)-Pb2(1:len);  [ zlPwM ]= find( (Pe2f-Pb2)<= 0) 
[ zlensort1170  zlensort1170I ] = sort(zlen1170,'descend');  
[ ERRxTw  ERRxTwI ]= max( Tef1(1:lTw)- Tb1f(1:lTw) )
i = 3; RO= double(RO) ;  RCu =[]; [ I ] = find(RO== Ra(ERRxTwI ));
EFILE=[BAL int2str(i) '_'  num2str(Afilemit) '.mat'];DFILE=fullfile(Pmit,EFILE)
A111 = genvarname(['inFj', Afilemits]);eval([A111 '= 1;' ] ) ;  
 if  exist( 'DFILE' , 'file'), AF = load( DFILE ) ; 
Ind =find(AF.zze1595csrows(:, 2 )== Afilemit);if  ~exist('R', 'var'), R=[]; end;  
R = [R; AF.zze1595csrows(Ind,1)]; RCu =[ RCu; AF.zze1595csrows(Ind, 3)]; 
end; if 0, R =Ra( zlensort1170I ); R=R1000( [ length( R1000):-1:1]) ;  end;
R =[ Ra( zlensort1170I(1:2) )' ] ;
end; if Afilemit == 100 , R= [  Ra( 231)  Ra(1907)  ] ; 
%       elseif Afilemit == 101 , 
elseif Afilemit == 114 , R= [  Ra( 1432  )  Ra( 1725)  ] ; 
 elseif Afilemit == 115 , R= [  Ra( 10 )   Ra( 65)  Ra( 30) Ra( 43)  Ra( 1624) ];
  elseif Afilemit ==116,R= [  Ra( 1502) Ra( 2030)  Ra(1290)   Ra( 1680) ...
  Ra( 2316) ]; 
  elseif Afilemit == 121 , R= [  Ra( 1017) ] ; 
  elseif Afilemit ==124,R= [Ra(387)  Ra( 934) Ra(1067)  Ra(1235)  Ra(1232)]; 
 
  elseif Afilemit ==201 , R= [Ra( 555) ] ;
  elseif Afilemit ==202 , R=setdiff(R   , [  Ra(1226)  Ra(2036)  ]')';  
      
 elseif Afilemit ==207, R= [Ra(238) Ra(101) Ra(75)  Ra( 1655)] ; 
  elseif Afilemit==208, R=[Ra(5) Ra(101) Ra(1854) Ra(1686) ...
          Ra(1773 ) Ra(1855 ) Ra(1802)] ;    
     elseif Afilemit == 209 , R= [  Ra( 5) Ra( 83) Ra( 510) Ra( 689)] ;    
elseif Afilemit == 210, R= [ R  Ra( 4)  Ra(1136) ] ; 
    
    elseif Afilemit ==214, R= [Ra(401)  Ra(1435) Ra(1453) ] ;               
  elseif Afilemit == 219,R= [Ra(387)] ;     dbclear at  2084 ; 
  elseif Afilemit ==222, R=[Ra(673)];
elseif Afilemit == 223, R= [ Ra(83)  Ra(162) Ra(191) Ra( 306) ...
 Ra( 503)  Ra(1307) Ra(2539)  Ra(1667)] ;
 elseif Afilemit == 233, R=[ Ra( 2)  Ra( 27)] ;  
  else R =[ Ra( zlensort1170I(1:2) )' ] ; %R=R1000( [ length( R1000):-1:1]) ;  
end;  if size (R,1)~=1 , R= R' ; end;
R= [ R  Ra( zlensort1170I(1:2))'  ];TESTL=length(R);  

end; if 10, for k =1:TESTL  if 1, Asubtyp=['Length' Afilemits ]; 
 I(k) = find(RO== R(k)) ; f = find( Ra ==R(k)) ; K2=f ;  fstr= int2str(f);
 zzlenA11(k) =f ; f= single(f); xb= [ int2str(f) ' R' num2str(Ra(f))] ; 
 %A11{ k}= int2str( zlensort1170(k) ) ;
 fmp2= (zlen1170(f) ) ;  A11{k } = int2str( zlen1170(f) ) ;   
  A11R{ k} =int2str(Ra( f )) ; stw=[ '' Afilemits  Ca(f )]; 
if f >= length(Ra)|| f==1, continue;  end; time= [ int2str(f-1) ];
if Afilemit == 200 & f== 1580 continue; end; 
 h_9_PT( k) = figure( 'Name' , [  '2070 Length'   ...
stw  A11{k} '  beat  ' xb   ' Rec'  lead1  A11{k} ] , ...
 'Position', [ screen(3)/2  0 screen(3)/2  400]);h =gcf;set( h,'WindowStyle','docked') ; 
if  f+1<=length(Ra),V=Ra(f +1)+ 90 ;else V = NS;  end;  U= Ra(f-1)-90 ;  
if Afilemit == 201 & f==554, U = Ra(f-1) - 190 ; V = Ra(f +1)+190 ; 
 end; if  (U<=0), U=1; end;  if  (V >= NS) V = NS ; end;U2 = U; V2 = V; 
fstr=int2str(f ) ; fsm1=  int2str(f-1) ; fsp1=  int2str(f+1 ) ;
 if  exist('xbl2', 'var') & exist('xbl1', 'var')& exist('xbl3', 'var'), N=3; m=0; end; 
 end; if exist('xbl1', 'var'), m=m+1 ; subplot(N,1, m );
 plot( U : V , xbl1( U : V) ,cyan , 'LineWidth', 2  ); iqrs= { 'FontSize', 12 } ;
 if BL,  line( [ U, V] , [0, 0] , 'Marker', '.'  , 'Color', 'k'  ) ; 
  elseif 1,  line([ U, V], [ 0,0 ] , 'Marker' , '.', 'Color', 'k' , 'LineWidth', 1);  
 end; if Afilemit == 232,mA=0.01; Ma= 0.01;
    maxxbl1= max( xbl1( U : V ) )+0.01;  minxbl1= min( xbl1( U : V))-0.01 ;
else mA=0.1;Ma=0.1; maxxbl1=max(xbl1(U: V))+0.1;minxbl1=min(xbl1(U:V))-0.1;
end; ylim([ minxbl1  maxxbl1 ]) ;  atyp= [  U:fix((V-U)/2):V] ;
antyp= [ minxbl1 :(maxxbl1-minxbl1)/2: maxxbl1] ; 
axis([ U V  minxbl1-0.1 maxxbl1+0.1]); set(gca,  'Units','normalized',  ...  
  'YTick', minxbl1-0.1:(maxxbl1- minxbl1)/2: maxxbl1+0.1,   ...
  'XTick', U:fix((V-U)/2):V,  'FontUnits',  'points', 'FontWeight',  'normal', ...
 'FontSize', fonts16 , 'FontName', 'Times' ) ;
CELL3y = minxbl1:(maxxbl1- minxbl1)/2: maxxbl1  ;      
for i=1:length( CELL3y)  [CELLstr{ i,1}, ccE2] =sprintf('%5.2f',CELL3y(i)); end;
  set(gca, 'YTick', CELL3y ); set(gca, 'YTickLabel', CELLstr);  
 set(gca, 'XTickLabel',atyp );iqrs= { 'FontSize', 12 } ;rafs=num2str(Ra(f))
titlelen=['Length ' A11{k} ' Rpeak ' num2str(Ra(f)) ' Record ' Afilemits ' beat ' fstr];
  zerrn2fm1= zerrw2( f-1);zerrn2f= zerrf2(f);zerrn2fp1= zerrA2(f+1);    
zerrnfm1= zerrA1(f-1) ; zerrnf= zerrw1(f);zerrnfp1= zerrf1(f+1) ; 
ym=maxxbl1-Ma; y=maxxbl1-Ma; yp=maxxbl1-Ma ;
CL=[ int2str(f) Ca(f) int2str(Ra(f))];AMP_S=[num2str(f) 'S' int2str(S(f)) ]; 
xbl1Raflm=xbl1(Ra(f-1));xbl1Rafl =xbl1(Ra(f)); xbl1Raflp=xbl1(Ra(f+1));
ta=['Channel 1 ' lead1s ' of class '  Ca(f ) ', beat ' fstr ', of Record ' Afilemits];
 title([ ta ] , 'color', 'r' ,  'FontSize' ,16 ); GPRL= int2str( K2 ) ;
 if track&&DBUG&&BL , dbstop 2105; dbstop 2106 ; end;  
 end; if 1, if  ( Afilemit== 100 ) ... 
 || (Afilemit==209 & (  f==717 || f== 716 || 1))  || (Afilemit==112) ... 
  ||  (Afilemit==208  & f==1770)  || (Afilemit==208  & f==168) ... 
  ||  ( Afilemit== 208  & f==155 ) || ( Afilemit== 208  & f==165 ) ... 
  || ( Afilemit== 208  & f==4 )   || ( Afilemit== 223  & f==1668 ) ... 
  || ( Afilemit== 207 & f==1655 )   || ( Afilemit== 223  & f==1668 ) ...
  
text( Ra(f )-2 , y , [  CL   ] ,'color', cb, 'FontSize',14) ;
   text(Ra(f-1)+2, ym , [ int2str(f-1) Ca(f-1) ]  ,'color', cr,'FontSize',14);
 text(Ra(f+1)-4, yp, [ int2str(f+1) Ca(f+1)] ,'color',cr,'FontSize', 14); 
elseif   ( Afilemit== 208  & f==155 ) || ( Afilemit== 208  & f==165 ) ... 
  || ( Afilemit== 208  & f==4 )   || ( Afilemit== 223  & f==1668 ) ... 
||  (Afilemit==114)||  (Afilemit==101) ... 
  text( Ra(f )-2 , y   , [  CL   ] ,'color', cb, 'FontSize',14) ;
   text(Ra(f-1)+2, ym , [ int2str(f-1) Ca(f-1) ]  ,'color', cr,'FontSize',14);
 text(Ra(f+1)-4, yp, [ int2str(f+1) Ca(f+1)] ,'color',cr,'FontSize', 14);   
elseif ( Afilemit==208  & f==1774 ) ||  (Afilemit==201 & f==1200 ) , ... 
    zerrnfm1= zerrA1(f-1) ; zerrnf= zerrf1(f)-0.0; zerrnfp1= zerrA1(f+1) 
    text( Ra(f )+2 , xbl1(Ra( f ))+zerrnf  , [  CL   ] ,'color', cb, 'FontSize',14) ;
text(Ra(f-1)+2,xbl1(Ra(f-1))+zerrnfm1,[ int2str(f-1) Ca(f-1)] ,'color',cr,'FontSize',14);
text(Ra(f+1)-1, yp, [ int2str(f+1) Ca(f+1)] ,'color',cr,'FontSize', 14); 
elseif  ( Afilemit==208  & f== 5 )  ... 
 zerrnfm1= zerrA1(f-1) ; zerrnf= zerrf1(f)-0.0; zerrnfp1= zerrA1(f+1)    
text(Ra(f-1)+2,xbl1(Ra(f-1))- zerrnfm1, [ int2str(f-1) Ca(f-1) ] ,'color',cr,'FontSize',14);
 text( Ra(f )+2 , xbl1(Ra( f ))-zerrnf  , [  CL   ] ,'color', cb, 'FontSize',14) ;
text(Ra(f+1)-1, maxxbl1-6*Ma, [ int2str(f+1) Ca(f+1)] ,'color',cr,'FontSize', 14);
     elseif ( Afilemit==202  & f==2953 ) ... 
             
text(Ra(f-1)+2,xbl1Rafl, [ int2str(f-1) Ca(f-1)],'color', cr,'FontSize',14);
 text( Ra(f )+2 , xbl1Rafl  , [  CL   ] ,'color', cb, 'FontSize',14) ;
text(Ra(f+1)-3,xbl1(Ra(f+1))+zerrnfp1, [ int2str(f+1) Ca(f+1)] ,'color',cr,'FontSize',14);
     elseif ( Afilemit==208  & f==2953 ) || ( Afilemit==201  & f==602 ) ...
 || ( Afilemit==201  & f==595 ) ,     
 text(Ra(f-1)+2,yp, [ int2str(f-1) Ca(f-1) ]  ,'color', cr,'FontSize',14);
 text( Ra(f )+2 , xbl1(Ra( f ))+0  , [  CL   ] ,'color', cb, 'FontSize',14) ;
 text(Ra(f+1)-1, yp, [ int2str(f+1) Ca(f+1)] ,'color',cr,'FontSize', 14); 
elseif ( Afilemit==208  & f==65 )  ,   
    text( Ra(f ) , yp  , [  CL   ] ,'color', cb, 'FontSize',14) ;
 text(Ra(f-1),yp-0.4, [ int2str(f-1) Ca(f-1) ]  ,'color', cr,'FontSize',14);
text(Ra(f+1)-3, yp-0.654, [ int2str(f+1) Ca(f+1)] ,'color',cr,'FontSize', 14);
elseif  ( Afilemit== 202  & f==1105 ) , 
    
 text( Ra(f ) , xbl1Rafl +zerrnf  , [  CL ] ,'color', cb, 'FontSize',14) ;
 text(Ra(f-1),xbl1(Ra(f-1))+ zerrnfm1, [ int2str(f-1) Ca(f-1)] ,'color',cr,'FontSize',14);
text(Ra(f+1)-3,xbl1(Ra(f+1))+zerrnfp1, [ int2str(f+1) Ca(f+1)] ,'color',cr,'FontSize',14); 
elseif (Afilemit==223) && f==2539,  
 text(Ra(f-1), ym+ zerrnfm1, [ int2str(f-1) Ca(f-1) ]  ,'color', cr,'FontSize',14);
    text( Ra(f ) ,ym  , [  CL   ] ,'color', cb, 'FontSize',14) ;
text(Ra(f+1)-3, xbl1(Ra(f+1))-zerrnfp1,[ int2str(f+1) Ca(f+1)],'color',cr,'FontSize',14);
elseif (Afilemit==223) ||  ( Afilemit== 201  & f==555 )   ,
     text( Ra(f ) , xbl1Rafl , [  CL   ] ,'color', cb, 'FontSize',14) ;
 text(Ra(f-1), xbl1Raflm, [ int2str(f-1) Ca(f-1) ]  ,'color', cr,'FontSize',14);
text(Ra(f+1)-3, xbl1Raflp , [ int2str(f+1) Ca(f+1)] ,'color',cr,'FontSize', 14);
elseif ( Afilemit==201  & f==566 ) ||  ( Afilemit==201  & f==602 )  
        
  text( Ra(f ) , xbl1Rafl +zerrnf  , [  CL   ] ,'color', cb, 'FontSize',14) ;
 text(Ra(f-1), xbl1(Ra(f-1))+ zerrnfm1, [ int2str(f-1) Ca(f-1) ]  ,'color', cr,'FontSize',14);
text(Ra(f+1)-3, xbl1(Ra(f+1))+zerrnfp1, [ int2str(f+1) Ca(f+1)] ,'color',cr,'FontSize',14);
elseif ( Afilemit==201  & f==602 )  
text( Ra(f ) , xbl1(Ra( f ))+zerrnf  , [  CL   ] ,'color', cb, 'FontSize',14) ;
 text(Ra(f-1), xbl1(Ra(f-1))+ zerrnfm1, [ int2str(f-1) Ca(f-1) ]  ,'color', cr,'FontSize',14);
text(Ra(f+1)-3, xbl1(Ra(f+1))+zerrnfp1, [ int2str(f+1) Ca(f+1)] ,'color',cr,'FontSize',14);
elseif ( Afilemit==234  & f==1306  )  
    text( Ra(f ) , xbl1(Ra( f ))+0  , [  CL   ] ,'color', cb, 'FontSize',14) ;
 text(Ra(f-1), xbl1(Ra(f-1))+zerrnfm1, [ int2str(f-1) Ca(f-1) ]  ,'color', cr,'FontSize',14);
text(Ra(f+1)-3,xbl1(Ra(f+1))+zerrnfm1, [ int2str(f+1) Ca(f+1)],'color',cr,'FontSize',14);
 elseif  (Afilemit==222 )  
     text( Ra(f ) , xbl1Rafl+0  , [  CL   ] ,'color', cb, 'FontSize',14) ;
 text(Ra(f-1), xbl1Raflm+ 0, [ int2str(f-1) Ca(f-1) ]  ,'color', cr,'FontSize',14);
text(Ra(f+1)-3, xbl1Raflp+0, [ int2str(f+1) Ca(f+1)] ,'color',cr,'FontSize',14);,
  elseif ( Afilemit==232 & f==1306  )  
   text( Ra(f ) , xbl1(Ra( f ))+0  , [  CL   ] ,'color', cb, 'FontSize',14) ;
 text(Ra(f-1), xbl1(Ra(f-1))+ zerrnfm1, [ int2str(f-1) Ca(f-1)],'color', cr,'FontSize',14);
text(Ra(f+1)-3, xbl1(Ra(f+1))+0, [ int2str(f+1) Ca(f+1)] ,'color',cr,'FontSize', 14);,
 elseif ( Afilemit==213 )  ||  (Afilemit==233)  || (Afilemit==222)  || 1
    text( Ra(f ) , xbl1Rafl+0  , [  CL   ] ,'color', cb, 'FontSize',14) ;
 text(Ra(f-1), xbl1(Ra(f-1))+ 0, [ int2str(f-1) Ca(f-1) ]  ,'color', cr,'FontSize',14);
text(Ra(f+1)-3, xbl1(Ra(f+1))+0, [ int2str(f+1) Ca(f+1)] ,'color',cr,'FontSize', 14);,     
end; if 0 %thesis empty 
    if (Tb1f(K2)> 0), text(Tb1f( K2) , xbl1( Tb1f( K2) ), ['Tb' ], 'color', 'b' );
 end; if (Tef1(K2)>0), text(Tef1(K2),xbl1(Tef1(K2)),['Te'] ,'color','b','FontSize', 14);
 end;    if (Pb2( f ) > 0),text( Pb2( f ) , xbl1( Pb2( f) ),[ 'Pb' ] ,'color', 'b' );
end;  if  ( Pe2f(f) > 0),  text(Pe2f( f) , xbl1( Pe2f( f) ), [   'Pe' ] ,'color', 'b' ); 
end; end; if ~Bthesis, if (Pe2fO(f)-Pb2O(f)>5), 
 text(Pb2O(f),xbl1(Pb2O(f)),['Pb' ],'color','b','FontSize',14);
if (Pe2fO(f)>0),text(Pe2fO( f) , xbl1( Pe2fO(f) ),[ 'Pe'] ,'color', 'b' , 'FontSize', 14);
end;end; if (1),text(Tb1fO(K2),xbl1(Tb1fO(K2)), [ 'Tb'] ,'color','b','FontSize',14);
end; if (Tef1O(K2)), text(Tef1O(K2),xbl1(Tef1O(K2)),['Te' ] ,'color', cb, 'FontSize',14);
end; if (Pb2O(f-1)>U),  text(Pb2O(f-1),xbl1(Pb2O(f-1)),['Pb'],'color',cr,'FontSize',14);
end; if (Pe2fO(f-1)>U), text(Pe2fO(f-1),xbl1(Pe2fO(f-1)),['Pe'] ,'color',cr ,'FontSize',14);
end; if (1), text(Tb1fO(K2-1), xbl1(Tb1fO(K2-1)), ['Tb'],'color',cr,'FontSize',14);
end; if (1), text( Tef1( K2-1), xbl1(Tef1(K2-1)), [ 'Te'] ,'color',cr,'FontSize',14);
end; if (Pb2O(f+1)>0), text( Pb2O( f+1), xbl1(Pb2O( f+1)),['Pb'],'color','r','FontSize',14);
end; if (1), text(Pe2fO(f+1),xbl1( Pe2fO(f+1)),['Pe'] ,'color',cr,'FontSize',14);
end;  end;  if ~Bthesis, if I(k)+1<= length(RO)& RO(I(k)+1)<V , 
  text(RO(I(k)+1), xbl1(RO(I(k)+1))  ...
, [ 'R' int2str(I(k)+1 ) C(I(k)+1) int2str(RO(I(k)+1))],'color',cr, 'FontSize', 14 );
end; if  I(k)-1 >=1 & RO(I(k)-1)> U , text(RO(I(k)-1),xbl1(RO(I(k)-1))-0.1 ,   ...
    [ 'R' int2str(I(k)-1) C(I(k)-1) int2str(RO(I(k)-1))],'color',cr , 'FontSize', 14 );
 end; if  I(k)-2 >=1 & RO(I(k)-2)> U,      text((RO(I(k)-2)), xbl1(RO(I(k)))-0.1  ,...
    [ 'R'  int2str(I(k)-2) C(I(k)-2) int2str(RO(I(k)-2))],'color', 'r' , 'FontSize', 14 );
end; end;  if ~Bthesis, if (( K2-1)> 0),
if (Pb2(f-1)>0),  text( Pb2(f-1), xbl1( Pb2( f-1)),[  'Pb' ] , 'color', 'r','FontSize', 14 );
end; if (Pe2f(f-1)>0),  text(Pe2f( f-1), xbl1(Pe2f( f-1)), [   'Pe'] ,'color', 'r'  );
end; if  (Tb1f( K2-1) ), text(Tb1f( K2-1), xbl1(Tb1f(K2-1) ),[  'Tb' ] , 'color', 'r'   ); 
end; if (Tef1(K2-1)>0), text(Tef1( K2-1) , xbl1(Tef1( K2-1)), [ 'Te' ] ,'color', 'r');
  end; end;  if  ( Q(f)  > 0),    text(Q( f) , xbl1( Q( f) ), [   'Q' ]  ,'color', 'b' );
end; text( Ra(f) ,xbl1(Ra(f )), [ CL] ,'color','b') ; text(S(f), xbl1(S(f)),[ 'S'], 'color','b' );
  if (( f+1)<=RL),   text( Ra(f+1), xbl1(Ra(f+1)),     ...
  [ int2str(f+1) Ca(f+1) int2str(Ra(f +1))] ,  'color','r'  , 'FontSize', 14  );
 if (Pb2(f+1)>0), text( Pb2(f+1), xbl1( Pb2(f+1)),[int2str(f+1) ' Pb'],'color', 'r' );
end;  if  (Pe2f(f+1)>0) text(Pe2f( f+1) , xbl1( Pe2f( f+1) ),[  'Pe'  ] ,'color', 'r' );
end;if (Tb1f(K2+1))&&(Tb1f(K2+1)<V),
    text(Tb1f(K2+1),xbl1(Tb1f(K2+1)),['Tb'],'color','r');
end; if ( Tef1(K2+1)> 0)&& (Tef1(K2+1)<V) 
 text(Tef1(K2+1) , xbl1( Tef1( K2+1) ),[ 'Tef1' ] ,'color', 'r' );
end;  end;  end; end; if  exist('xbl2', 'var'), m=m+1 ; subplot(N,1, m );  
set(gca,'Units','normalized', 'FontUnits', 'points', 'FontWeight',  'normal',...
  'FontSize', fonts16 ,'FontName', 'Times'); lead2str=[ ' Channel 2 ' lead2];
title([ ' Channel 2 ' lead2s ] ,'color','r' ,'FontSize',16);
 if exist('xbl2' , 'var'),  plot( U2 : V2 , xbl2( U2 : V2),cm, 'LineWidth', 2 );   
  if BL,   line( [ U2,V2] , [0,0] , 'Marker' ,   '.'  ,  'Color', 'k' ); 
 elseif 1,  line([ U, V], [ 0,0 ] , 'Marker' , '.', 'Color', 'k' , 'LineWidth', 1);  
  end; if (Afilemit == 101 && f== 176) || (Afilemit == 209)|| (Afilemit ==212) ,
minxbl2xbf1=min( [xbl2( U2 : V2)])-0.02;  maxxbl2xbf1=max([xbl2(U2: V2)])+0.02; 
else  minxbl2xbf1=min([xbl2( U2: V2)])-0.1; maxxbl2xbf1=max([ xbl2(U2:V2)])+0.1;
 end; xdfcp=[  minxbl2xbf1 :(maxxbl2xbf1- minxbl2xbf1)/2:maxxbl2xbf1 ] ;
% set(gca,  'Units','normalized' , ...
%'YTick', minxbl2xbf1 :(maxxbl2xbf1- minxbl2xbf1)/2:maxxbl2xbf1); 
 axis([ U V  minxbl2xbf1  maxxbl2xbf1 ]);
  set(gca,'XTick', U:fix((V-U)/2):V);  set(gca,  'XTickLabel', (get(gca, 'XTick')) ) ;
CELL3y = minxbl2xbf1 :(maxxbl2xbf1- minxbl2xbf1)/2 : maxxbl2xbf1 ;
for i= 1: length(CELL3y) [ ST1adc{ i,1}, celE2] =sprintf('%5.2f',CELL3y(i));end; 
 set( gca,  'YTick', CELL3y);  set( gca,    'YTickLabel', ST1adc );    
  title( [lead2str] ,'color','r' ,'FontSize',16 ); CL= [ int2str(f)  Ca(f) int2str(Ra(f ))] ; 
 if bon,if  Pe2f(f)-Pb2(f)>20, text(Pb2(f) ,xbl2(Pb2(f)),[ 'Pb'] ,'color','b','FontSize',14);
 text( Pe2f( f  ) ,  xbl2( Pe2f( f  ) ),  [ 'Pe'  ] , 'color', cb ,'FontSize', 14); 
 end;   text( Tb1fO( f ) , xbl2( Tb1fO( f) ),  [ 'Tb' ] , 'color', 'b' ,'FontSize', 14);
    text( Tef2( f ) , xbl2( Tef2( f) ),[   'Te'    ] , 'color', 'b' ,'FontSize', 14);
end; if ~Bthesis, if  Pb2( f-1)>U,
text( Pb2(f-1) ,  xbl2( Pb2(f-1)),  [ 'Pb' ] , 'color',cr,'FontSize', 14 );
end;  if  Pe2f( f-1 )>U,    text( Pe2f( f-1) ,  xbl2( Pe2f( f-1  ) ),  [ 'Pe'  ] , 'color', cr ); 
end; text( Tb1fO( f-1 ) ,  xbl2( Tb1fO( f-1) ),  [ 'Tb' ] , 'color', cr ,'FontSize', 14);
     text( Tef2( f-1 ) , xbl2( Tef2( f-1) ),[   'Te' ] , 'color', 'r' ,'FontSize', 14);     
     text( Pb2( f  +1) ,  xbl2( Pb2( f +1) ),  [ 'Pb' ] , 'color', cr , 'FontSize', 14);
     text( Pe2f( f  +1) ,  xbl2( Pe2f( f +1) ),  [ 'Pe'  ] , 'color', cr ,'FontSize', 14);
end;  end; end; if  exist('xbl3', 'var'), m=m+1;   subplot( N,1, m  );  
 plot(U2:V2,xbl3(U2 : V2) , 'g' , 'LineWidth',2);CL60=['Channel 1 + Channel 2'];
if BL, line( [ U2,V2] , [0,0] , 'Marker' , '.'  ,'Color', 'k' );
elseif 1,  line([ U, V], [ 0,0 ] , 'Marker' , '.', 'Color', 'k' , 'LineWidth', 1);  
end;  title([ CL60] ,'color','r' ,'FontSize',16); zlPwCm1C=U:fix((V-U)/2):V;
 set(gca,'XTick', U:fix((V-U)/2):V );  set(gca, 'XTickLabel',zlPwCm1C ) ;
 if Afilemit == 201 || Afilemit == 208 , 
  minxf3=min( [xbl3(U2 : V2 )])-0.1;maxf3=max([ xbl3( U2 :V2)])+0.1;
 else  minxf3= min( [xbl3( U2 : V2 )])-0.01;  maxf3=max( [ xbl3( U2 : V2)] )+0.01;
 end; GPRLN = minxf3 :(maxf3- minxf3)/2 : maxf3;  time=  [ int2str(f-1) ]; 
 for i=1: length(GPRLN) [ GPC2c{ i,1}, celTB2]=sprintf('%5.2f',GPRLN(i));end;  
set( gca,'YTick',GPRLN );set( gca,'YTickLabel', GPC2c); axis([U V minxf3 maxf3]);
   set(gca,  'Units','normalized', 'FontUnits', 'points', ...
  'YTick', GPRLN  , 'XTick', U:fix((V-U)/2):V, ...
 'FontWeight',  'normal', 'FontSize', fonts16 , 'FontName', 'Times'); 
CL =[ int2str(f)  Ca(f) int2str(Ra( f ) ) ] ; TIM= [ '' ] ; fsm1= int2str(f-1);
if 0, text( Ra(f)+2 , xbl3(Ra( f ) ) , [ CL ] ,'color',   cb ,'FontSize', 14) ; 
text(Ra(f-1),xbl3(Ra(f-1)),[ int2str(f-1) Ca(f-1) int2str(Ra(f-1))],'color',ck,'FontSize',14);
text(Ra(f+1)+2, xbl3(Ra(f+1)), [ int2str(f+1) Ca(f+1) int2str(Ra(f+1))] ,'color',ck );
if I(k)+1<= length(RO), text( RO(I(k)+1 ), xbl3(RO(I(k)+1 ))-0.0  ...
, ['R' int2str(I(k)+1 ) C(I(k)+1) int2str(RO(I(k)+1 ))],'color','r' ,'FontSize', 14);
end;  if  I(k)-1 >=1,    text(RO(I(k)-1), xbl3(RO(I(k)-1))-0.0 ,   ...
    [ 'R' int2str(I(k)-1) C(I(k)-1) int2str(RO(I(k)-1))], 'color','r' ,'FontSize', 14  );
 end; end; if  0, if  I(k)-2 >=1 &&RO(I(k) -2)>U, 
 text((RO(I(k)-2)),xbl3(RO(I(k) -2))-0.0 ,...
 [ 'R'  int2str(I(k)-2)    C(I(k)-2) int2str(RO(I(k)-2))], 'color','r','FontSize', 14);
end;  end; if  bon, if  (Pe2f(f)-Pb2( f ) >15 )   if  (Pb2( f)>0), 
text( Pb2( f ) , xbl3( Pb2( f) ),[ 'Pb' ] , 'color', 'b' ,'FontSize', 14);
end; if  (Pe2f(f)>0),  text(Pe2f( f) , xbl3( Pe2f( f) ),[ 'Pe' ] ,'color', 'b' ,'FontSize', 14); 
end;end; if (Tb1f(K2)),text(Tb1f( K2) , xbl3(Tb1f(K2)),[ 'Tb' ],'color','b' ,'FontSize',14);
 end; if ( Tef3(K2)> 0),  GPRL=   int2str( ( K2) ) ;
 text(Tef3( K2) , xbl3( Tef3( K2) ), [   'Te'   ] ,'color', 'b','FontSize', 14 );
 end; end;  if ~Bthesis, if (( f-1)> 0), 
 if  Pb2( f-1)>U, text( Pb2(f-1) , xbl3( Pb2( f-1) ),[   'Pb' ] , 'color', 'r','FontSize',14 );
end; if (Pe2f(f-1)>U), text(Pe2f(f-1), xbl3(Pe2f( f-1)),[ 'Pe' ] ,'color', 'r' ,'FontSize',14);
end; if (Tb1f(K2-1)),text(Tb1f(K2-1), xbl3(Tb1f(K2-1)),['Tb'] ,'color','r' ,'FontSize',14); 
end; if 1,  text(Tef3( K2-1) , xbl3( Tef3( f-1)) ,['Te' ] ,'color', 'r','FontSize', 14 );
end; end;  if 0,  if  ( Q(f)> 0),    text(Q( f) , xbl3( Q( f) ),[ 'Q'] ,'color', 'b' );
end;   text(  S(f) , xbl3(S(f ) )  ,  [ 'S' ], 'color',  'b'  ,'FontSize', 14 );     
end; if ((f+1)<=RL), if (Pb2(f+1)>0),
  text( Pb2(f+1), xbl3( Pb2(f+1)),[  'Pb'],'color','r','FontSize', 14);
end; if (Pe2f(f+1)>0),text(Pe2f( f+1) , xbl3(Pe2f(f+1)),['Pe'],'color', 'r' ,'FontSize', 14);
end;  if ( Tb1f( K2+1)>0)&& ( Tb1f( K2+1) < V) 
    text(Tb1f(K2+1) , xbl3( Tb1f( K2+1)), [ 'Tb' ], 'color', 'r','FontSize',14 );
end; if (Tef1(K2+1)> 0)&&  ( Tef1( K2+1) < V) 
 text(Tef3( K2+1) , xbl3( Tef3( K2+1) ),[  'Te'  ] ,'color', 'r','FontSize',14 );
end;  end;  end; if ~Bthesis, if ((K2-1)> 0), 
 text(Tb1fO(K2-1), xbl3(Tb1fO( K2-1 ) ), ['Tb'] , 'color', cr);
end; if (Tef1O(K2-1)>0), text( Tef1O( K2-1), xbl3( Tef1O(K2-1)) , ['Te' ] ,'color', cr );
end; if  bon, if (Pe2fO(f)- Pb2O(f) >10),
 text( Pb2O(f) , xbl3(Pb2O( f) ),  ['Pb'] , 'color', 'b' , 'FontSize', 14); 
text(Pe2fO(f), xbl3( Pe2fO(f) ),[ 'Pe'] ,'color', 'b'  ,'FontSize', 14 );
end;if (Tb1fO(K2)>0),text(Tb1fO(K2),xbl3(Tb1fO(K2)),['Tb'] ,'color','b','FontSize',14);
end; if (Tef1O(K2)),text(Tef1O(K2), xbl3(Tef1O(K2)), ['Te' ] ,'color', cb,'FontSize',14);
end;  if (Pe2fO(f+1)-Pb2O(f+1)>20), % h_9_PTs(k)=  figure( h_9_PT(k))
  text( Pb2O( f+1), xbl3(Pb2O( f+1) ),  ['Pb'] , 'color', 'r' , 'FontSize', 14 ); 
   text(Pe2fO( f+1), xbl3( Pe2fO(f+1)), [ 'Pe'] ,'color', cr ,'FontSize', 14 );
end;end;end;end; Ns=int2str(N);
GP2nan=[d(1:2),d(4:6),d(10:11),d(13:14),d(16:17)];
FIGFILE=[Asubtyp Ns fstr Ca(f) dates B '.png'];FIGUFILE=fullfile(PATFIG,FIGFILE);  
if  DBUG ,dbstop 2312;
if 10, saveas(h_9_PT(k), [ FIGUFILE ] ); end; %delete( [ FIGUFILE ] )
end; end; if 1, [ zlensort1170 zlensort1170I ]=sort(zlen1170,'descend');      
[ zlen1170M, zlen1170Mi ] =max(zlen1170) ;[ zlensort1820] =mean(zlen1170); 
sz =std(zlen1170) ; [ zlenm3s ] =find(zlen1170 > zlensort1820+3*sz);      
zzlen =[ zzlenA11  ] ; zlens =zlensort1170I(1:2);
for i=1: length(zzlen)  zsleft =zzlen(i); Pe2f(zsleft)=Q(zsleft)-4; 
Pb2(zsleft)=Q(zsleft)-8; Tb1f(zsleft)= S(zsleft)+8; 
Tef1old(i)=Tef1(zsleft);Tef1(zsleft)=S(zsleft)+14;Tef1diff(i)=Tef1old(i)-Tef1(zsleft);
end; end; end;end;  if 1, fprintf(' 2320 Reprod %s\n', uC );    
r1000r= size(R1000,1);  cra=  size(Ra,2); iqrs={ 'FontSize',14}
if r1000r~=1, R1000 = R1000'; end;  if cra~= 1, Ra= Ra'; end; 
if Afilemit ==100, R1000= [R1000  Ra(2273)   258198 435895 ];
elseif Afilemit ==103, R1000= [R1000  Ra(1268)    ];
elseif Afilemit ==105, R1000= [R1000  24842  258198 435895 ];
elseif Afilemit == 106,R1000=[R1000 Ra(104) Ra(121) Ra(906) Ra(914)] ; 
% [ Ra( 972,1227,1298) ] ;%c= setdiff(A, B)
elseif Afilemit == 108, R1000= [R1000 436069  Ra(Pe2fQmaxI) Ra(1747)] ;
  elseif Afilemit == 111, R1000= [ R1000      296844 165994  ] ;
elseif Afilemit == 113, R1000= [R1000  Ra(1318 )  Ra(1313) ] ; 
          
 elseif Afilemit == 115, R1000= [R1000   Ra(1446  ) Ra( 43 )    ] ;          
elseif Afilemit ==116, R1000= [ Ra(128) Ra(386) Ra(1318)  Ra(2316)  Ra(2412)] ;  
elseif Afilemit ==121, R1000= [ R1000  Ra([1553 1521 1844 366 1052 1565]')'] ;
elseif Afilemit == 123   R1000=    [ R1000   152283 ] ;    
  elseif Afilemit == 124,   R1000= [ R1000    Ra( 203)   Ra( 704)] ;      
elseif Afilemit ==200, R1000=[R1000 Ra(1580) 153826 549292 125923 379898];
     elseif Afilemit == 201,  R1000= [ Ra( 1528)     Ra( 449) Ra( 704) ...  
  Ra(555)  Ra( 609) Ra(624)    Ra(743)  Ra(882)  Ra(1609)];  
 R1000 = [ Ra( 1528)   Ra(555)    Ra( 449) Ra( 704) ]
elseif Afilemit==202,R1000=[Ra(356) Ra(745) Ra(1103) Ra(687) ...
Ra(713) Ra(1105)];   R1000 = setdiff([  R1000 ]  , [  Ra(680)  ]')';  
elseif Afilemit == 203,  R1000= [ R1000    Ra(171) 378737 ] ;   % Ra(1516)
 elseif Afilemit == 205,  R1000= [R1000  Ra(2181) Ra(2189)  ]   ;             
elseif Afilemit == 207,R1000= [R1000 Ra(1645) Ra(1658) Ra(1752)...
Ra(1646)    Ra(1647)  590585 604050] ;[Ra(1770) Ra(1774)]
R1000= [ 592308 ] ; Ra(1647) 
elseif Afilemit ==208,R1000=[ R1000 25874 499292  547973 Ra(1803)  ]; 
 R1000= [Ra(1803) Ra(2296) 498665 438292  385263  385989 Ra(126) Ra(101)] ;
   elseif Afilemit == 209, R1000=  [ R1000  Ra([83 158    510 689]'  )' ]; 
elseif Afilemit == 213, R1000= [ R1000  Ra(3251) 279271 174268  ...
282458   381497 225777  377477  Ra(310)] ; %214 1667 pwave exceeds131
 elseif Afilemit == 214, R1000=  [ Ra([1667]' )' Ra( 476) Ra(566 )  Ra( 203) ]  ; 
 elseif Afilemit == 220, R1000=  [ Ra([1395]')' R1000 ]  ; 
     elseif Afilemit == 220, R1000=  [ Ra([7]')' R1000 ]  ;  
elseif Afilemit==222, R1000=[ Ra(675) Ra(673)  Ra(801) ...
  Ra(2128) ]; Ra(655)
elseif Afilemit ==223,R1000= [Ra(32) Ra(191) Ra(1307) Ra(162) Ra(86) R1000...
 Ra(2539) Ra(1667) Ra(1668)  415707 ];
elseif Afilemit == 232,R1000= [ R1000 342973  232433 228931 142161 ...
 344483 966   Ra(391) 142158    Ra(1249)  ];         
 elseif Afilemit == 233, R1000= [R1000  630601 184430  1565 3013 Ra(27) ...
113775 122979 ;  ] ; % thesis % replace 0 by 1
elseif Afilemit == 234,   R1000=  [ R1000  Ra(1303)  ]; 
end; %if track&DBUG&BL  dbstop 2367; end;   
if isempty(R1000),R1000=[Ra(zlensort1170I( hwan)) ]; end; R1000=unique(R1000);
if  length(R1000) >= 10, BT=10; else BT= length( R1000);end; 
end; for  i = 1 : BT   if 1, 
f =find(Ra==R1000(i)) ; I=find(RO==R1000(i) ); 
if  length(f )>1,  f1= f(1) ;  f2=f(2);  I1 = I(1);I2 = I(2);f= f(1); I = I(1) ;
 U = Ra(f1-2) - 20;  V = Ra( f2 +2) + 50 ; 
elseif  length(f) ==1, if f+1>= length(Ra)|| ( f-1<=0) , continue; end; 
U =Ra(f -1) - 50 ;   V= Ra(f +1)+150 ; 
else  continue;  
end; if (Afilemit== 207) && (f == 238), U= Ra( f )-120;V= Ra( f)+120;
    U=Ra(f -1) - 50 ;    V= Ra(f +1)+150 ; 
elseif (Afilemit ==207) && (f ==2229),U= Ra( f )-120;V= Ra( f)+120;
elseif (Afilemit == 207) && (f==1645), U=Ra(f)-90;V=Ra(f)+90;
elseif (Afilemit == 203), U = Ra( f -1) - 30 ;   V = Ra( f +1)+ 30 ; 
elseif  (Afilemit == 200) && (f==2229), U=Ra( f-1)-20; V= Ra(f +1)-20;V=Ra(f+1)+90;     
elseif  (Afilemit == 223) && (f==2539), U=Ra( f-1)-150; V= Ra(f +1)+150; 
end; if  (V >= NS) V = NS ; end; if  (U <=0), U= 10; end; 
if (Afilemit== 207) && 1, U= Ra( f-1 )-150;V= Ra( f+1)+150;end; 
F3= [ 'Reprod' Afilemits 'Beat ' int2str(f) 'Beat R' int2str(R1000(i)) Ca(f) ] ;
if ~isempty( zn0 ),  ff  = find( z0minksR == R1000(i) ) ; 
if ff, F3= ['Reprod' Afilemits 'Beat ' int2str(f) 'Beat R' int2str(R1000(i)) Ca(f) ];
end; end; if Afilemit == 208 & f== 2296 continue;
elseif Afilemit == 208 & f== 2012,  continue;
elseif Afilemit == 208 & f==2292,continue; 
end; m=0;  K2 = f ; time=  [ int2str(f )   ]; atyp= int2str(zlen1170(f) ) ;
 if  exist('xbl2', 'var') & exist('xbl1', 'var')& exist('xbl3', 'var'), N=3;end; 
 if (U<=0) U=10; end; if (V>= NS),V =NS -10;end;    
  xb= [int2str(f+2) Ca(f+2) int2str(Ra(f+2))] ;  if 1, N=2; else N=3; end; 
 repflag= ~isequal(Ca(f),'N') & ~isequal(Ca(f),'Q'); 
    end; if repflag, h_940(i) = figure( 'Name', [ '2400'   F3  ] ,  'Position', ...
 [10+i*50 10 screen(3)/2 screen(4)-100]);h=gcf;set(h,'WindowStyle','docked'); 
 if  exist('xbl1', 'var'), if 1,m=m+1; subplot(N,1, m ); 
 plot( U : V, xbl1( U : V) ,cm  , 'LineWidth', 2 ); hold on;  
 if BL, line( [ U, V] , [0, 0] , 'Marker', '.'  , 'Color', 'k');   
 elseif ~BL& Afilemit==213 , line( [ U, V] , [0, 0] , 'Marker', '.'  , 'Color', 'k');  
  elseif 1,  line([ U, V], [ 0,0 ] , 'Marker' , '.', 'Color', 'k' , 'LineWidth', 1);     
 end;  [ minxbf1uv] = min(xbl1( U : V))-0.1;[ maxxbf1uv] =max(xbl1( U : V))+0.1;  
Mr= 0.1; axis([ U V  minxbf1uv maxxbf1uv]); 
 set( gca, 'Units' , 'normalized', 'XTick', U:fix((V-U)/2):V,  'FontUnits', 'points', ......
 'FontWeight', 'normal', 'FontSize', fonts16, 'FontName', 'Times');
 ROM =[ minxbf1uv:(maxxbf1uv- minxbf1uv)/2 : maxxbf1uv ] ;     
 for rom = 1: length( ROM) [ ST1{rom,1}, errsg] =sprintf('%.2f',ROM(rom));end;
 set( gca,  'YTickLabel',  ST1 )  ;  
 set( gca, 'YTick', minxbf1uv:(maxxbf1uv- minxbf1uv)/2:maxxbf1uv,...
  'XTick', U:fix((V-U)/2):V   );  set(gca,  'XTickLabel', (get(gca, 'XTick'))  ); 
 TIM= [ '' ] ; xb= [int2str(f ) Ca(f ) int2str(Ra(f ))] ;
fprintf('2420 %d %c Pb%d Pe%d Tb%d Te%d Pb%d Pe=%d\n', ...
 f ,  Ca(f), Pb2(f ), Pe2f(f ),Tb1f(f ) , Tef1( f ), Pb2(f+1), Pe2f(f+1) );
 RA2= [int2str(f+1) Ca(f+1)] ;QTC= int2str(Ra(f-1));
 TeS3Tef3 = [ int2str(f-1) Ca(f-1) int2str(Ra(f-1)) ]; 
 if Afilemit==113,zerr43fm1= zerrA1(f-1) ; zerr43f= zerrw1(f);zerr43fp1= zerrf1(f+1); 
elseif Afilemit==202&&f==26, 
    zerr43fm1=zerrA1(f-1);zerr43f=zerrw1(f);zerr43fp1=zerrf1(f+1);
elseif Afilemit==208,zerr43fm1= zerrA1(f-1); zerr43f= zerrA1(f);zerr43fp1=zerrf1(f+1);
elseif Afilemit==201,zerr43fm1= zerrA1(f-1); zerr43f= zerrA1(f);zerr43fp1=zerrf1(f+1);
 else zerr43fm1=zerrA1(f-1) ; zerr43f= zerrw1(f);zerr43fp1= zerrf1(f+1) ;    
end; xbl1Rafrm=xbl1(Ra(f-1)); xbl1Rafr=xbl1(Ra(f));xbl1Rafrp=xbl1(Ra(f+1)); 
title(['Channel 1 ' lead1s ' of  class '  Ca(f) ', Beat ' int2str(f) ', Record ' Afilemits ]...
,'color', 'r' , 'FontSize' ,16 );
if DBUG&BL,  dbstop 2426; end;   
 end; if (Afilemit==202 &&f==26) || (Afilemit==114)
     
text(Ra(f),xbl1(Ra(f)+1)+zerr43f,[ int2str(f) Ca(f) int2str(Ra(f))],'color',cb,'FontSize',15);
text( Ra(f-1),xbl1(Ra(f-1))+zerr43fm1, [ int2str(f-1) Ca(f-1) ], 'color' , 'r' , 'FontSize',15);
text(Ra(f+1), xbl1(Ra(f+1))+zerr43fp1,[ int2str(f+1) Ca(f+1) ],'color','r', 'FontSize',15);   
 elseif  BF&&(Afilemit== 201 &&f==73) , 
text(Ra(f)+1,xbl1(Ra(f))+zerr43f,[int2str(f) Ca(f) int2str(Ra(f))],'color','b','FontSize',15);
text( Ra(f-1), xbl1(Ra(f-1))+zerr43fm1 , [ int2str(f-1) Ca(f-1)], 'color','r' , 'FontSize',15);
text(Ra(f+1), xbl1(Ra(f+1))+zerr43fp1, [ int2str(f+1) Ca(f+1) ],'color','r', 'FontSize',15);   
elseif  (Afilemit== 212 )  || (Afilemit== 208 &&f==1770 ) ...  
 || (Afilemit== 234 &&f==1303 ) 
text(Ra( f), maxxbf1uv-Mr, [ int2str(f ) Ca(f ) int2str(Ra( f))],'color', 'b', 'FontSize',15);
  text( Ra(f-1), maxxbf1uv-Mr , [ int2str(f-1) Ca(f-1)   ], 'color' , 'r' , 'FontSize' ,15);
text(Ra(f+1), maxxbf1uv-Mr ,    [ int2str(f+1) Ca(f+1) ],'color','r', 'FontSize',15);   
 elseif  BF &&(Afilemit== 208 &&f==1774) 

text(Ra(f)+1,xbl1(Ra(f))+zerr43f, [int2str(f) Ca(f) int2str(Ra(f))],'color','b','FontSize',15);
 text( Ra(f-1), xbl1(Ra(f-1))+zerr43fm1 , [ int2str(f-1) Ca(f-1)], 'color', 'r', 'FontSize' ,15);
text(Ra(f+1) ,ym,    [ int2str(f+1) Ca(f+1) ],'color','r', 'FontSize',15);    
elseif  BF && ( (Afilemit== 223 &&f==32) ) 
    
 text(Ra( f ), xbl1Rafr+0.2 , [ int2str(f ) Ca(f ) int2str(Ra( f))],'color', 'b' , 'FontSize' ,15);
  text( Ra(f-1), maxxbf1uv-Mr , [ int2str(f-1) Ca(f-1)   ], 'color' , 'r' , 'FontSize' ,15);
text(Ra(f+1), xbl1Rafrp ,    [ int2str(f+1) Ca(f+1) ],'color','r', 'FontSize',15);    
 elseif  ~BF &&  (Afilemit== 207 &&f==2)   || (Afilemit== 207&&f==38)       
     
 text( Ra(f-1)+3, xbl1(Ra(f-1))+0.0 , [ int2str(f-1) Ca(f-1) ], 'color' , cr , 'FontSize' ,15);
 text(Ra(f )+5,xbl1(Ra(f))+0.0,[ int2str(f) Ca(f) int2str(Ra(f))],'color','b' , 'FontSize',15);
text(Ra(f+1)+4, xbl1(Ra(f+1))+0.0 ,  [ int2str(f+1) Ca(f+1) ],'color',cr, 'FontSize',15);   
 elseif   (Afilemit== 213 &&f==112)  ...
|| (Afilemit== 213 &&f==112)  || (Afilemit== 213)   
 text( Ra(f-1)+3, xbl1Rafrm , [ int2str(f-1) Ca(f-1)   ], 'color' , cr , 'FontSize' ,15);
 text(Ra( f )+5,xbl1(Ra(f))+0, [ int2str(f) Ca(f) int2str(Ra( f))],'color','b' , 'FontSize',15);
text(Ra(f+1)+4, xbl1Rafrp+0.0 ,  [ int2str(f+1) Ca(f+1) ],'color',cr, 'FontSize',15);  
 elseif  (Afilemit== 208 &&f==101 )   
  text( Ra(f-1)+3, xbl1Rafrm , [ int2str(f-1) Ca(f-1) ], 'color' , cr , 'FontSize' ,15);
 text(Ra( f )+5,xbl1Rafr+00, [ int2str(f ) Ca(f) int2str(Ra( f))],'color','b' , 'FontSize',15);
text(Ra(f+1)+4, xbl1Rafrp+0.0 ,  [ int2str(f+1) Ca(f+1) ],'color',cr, 'FontSize',15);  
elseif (Afilemit== 201 &&f==449 )  || (Afilemit== 201 &&f==449 )    
text( Ra(f-1)+3,  maxxbf1uv-Mr+0 , [ int2str(f-1) Ca(f-1) ], 'color' , cr , 'FontSize',15);
text(Ra(f)+5,maxxbf1uv-Mr+0, [ int2str(f) Ca(f) int2str(Ra(f))],'color','b' , 'FontSize',15);
text(Ra(f+1)+4,  maxxbf1uv-Mr+0.0,  [ int2str(f+1) Ca(f+1) ],'color',cr, 'FontSize',15);    
elseif (Afilemit== 201 &&f==609)  || (Afilemit== 201 &&f==624 )...
     || (Afilemit== 209 &&f==83 )  || (Afilemit== 203 &&f==11 )
  text( Ra(f-1)+3, xbl1Rafrm+0.23  , [ int2str(f-1) Ca(f-1) ], 'color' , cr , 'FontSize',15);
text(Ra( f )+5,xbl1Rafr+0.0, [ int2str(f ) Ca(f) int2str(Ra( f))],'color','b' , 'FontSize',15);
text(Ra(f+1)+4, xbl1Rafrp+0.2 ,  [ int2str(f+1) Ca(f+1) ],'color',cr, 'FontSize',15);    
elseif  (Afilemit== 201 &&f==704)  
  text( Ra(f-1)+3, xbl1Rafrm+0.0 , [ int2str(f-1) Ca(f-1)   ], 'color' , cr , 'FontSize' ,15);
text(Ra( f )+5,xbl1Rafr+0.0, [ int2str(f ) Ca(f) int2str(Ra( f))],'color','b' , 'FontSize',15);
text(Ra(f+1)+4, xbl1Rafrp+0.0 ,  [ int2str(f+1) Ca(f+1) ],'color',cr, 'FontSize',15);     
else   text( Ra(f-1)+3, xbl1Rafrm , [ int2str(f-1) Ca(f-1)   ], 'color' , cr , 'FontSize' ,15);
text(Ra(f )+5,xbl1(Ra(f) )+0.1,[ int2str(f ) Ca(f) int2str(Ra(f))],'color','b' , 'FontSize',15);
text(Ra(f+1)+4, xbl1(Ra(f+1))+0.0, [ int2str(f+1) Ca(f+1)],'color',cr, 'FontSize',15);    
end; pwf(k) = (Pe2f(f)-Pb2(f)) ; 
if 0&  ((Afilemit ==103 & f==1268) ...  
|| (Afilemit ==214 & f==1667 )||0 ) , 
 if track,  text( Pb2( f ) ,  xbl1( Pb2( f)),  [ 'Pb' ] , 'color', 'b' );
text( Pe2f(f ) ,  xbl1( Pe2f( f ) ),  [ 'Pe'  ] , 'color', cb );
end; if   exist('Pb2M','var')&& (~isnan(Pb2M(f)))&&(f<=length(Pb2M))
 text( Pb2M(f) , xbl1( Pb2M(f) ),  [ 'PbM' ] , 'color', 'b' );
end; if  exist('Pe2fM', 'var') &&(~isnan(Pe2fM( f )))&&(f<=length(Pe2fM))
  text( Pe2fM( f ) , xbl1( Pe2fM( f) ),  [ 'PeM' ] , 'color', 'b' );
end; end;  if ~Bthesis, if (Pe2f(f)-Pb2(f))>20,
 text( Pb2(f), xbl1(Pb2(f)),[ 'Pb' ] , 'color', 'b','FontSize',15);  
  text(Pe2f(f ) , xbl1( Pe2f( f )),  [ 'Pe' ] , 'color', 'b', 'FontSize',15); 
 end;  
 if 1,  text(Tb1f(f  ),  xbl1(Tb1f(f )), [ 'Tb'  ], 'color',   cb , 'FontSize' ,15); 
 text(Tef1(f),  xbl1(Tef1(f  )),  [ 'Te'  ], 'color',  cb , 'FontSize' ,15); 
end; if (( K2-1) > 0), if  ( Pb2(f-1)>0 && Pb2(f-1)>=U ),
  text( Pb2(f-1 ) , xbl1( Pb2( f-1) ), ['Pb' ] , 'color', 'r' , 'FontSize' ,15  );
 end; if (Pe2f(f-1)>0  &&Pe2f(f-1)>U  ),    time=  [ int2str(f ) ];
  text( Pe2f( f-1), xbl1(Pe2f( f-1)), [ 'Pe'] ,'color', 'r' , 'FontSize' ,15 );
 end; if (Tb1f(K2-1)), text(Tb1f(K2-1), xbl1(Tb1f(K2-1)),['Tb' ] , 'color', 'r','FontSize',15);
end; if (Tef1(K2-1)), text(Tef1( K2-1) , xbl1(Tef1(K2-1)),[ 'Te' ] ,'color', 'r','FontSize',15);
end; end;  if (( f+1)<=RL),   if (Pb2(f+1) >0 && Pb2(f+1)<=V ),
 text( Pb2(f+1), xbl1( Pb2(f+1)),[ ' Pb'], 'color', 'r' , 'FontSize' ,15);
end; if (Pe2f(f+1)>0&& Pe2f(f+1)<=V),
text(Pe2f( f+1), xbl1(Pe2f(f+1)) , [ 'Pe'] ,'color', 'r' ,'FontSize' ,15);
end;  if ( Tb1f( K2+1)>0  && Tb1f(f+1)<=V)  
 text(Tb1f(K2+1) , xbl1( Tb1f( K2+1) ), [   'Tb' ], 'color', 'r' , 'FontSize' ,15);
end;    if ( Tef1( K2+1)> 0)&&  ( Tef1( K2+1)< V) 
 text(Tef1( K2+1) , xbl1( Tef1( K2+1) ),[ 'Te' ] ,'color', 'r', 'FontSize' ,15);
end;  end; end; if DBUG&&~Bthesis,
if  I(k)-2 >=1,text((RO(I(k)-2)),xbl1(RO(I(k)-2))-0.0,...
 [ 'R' int2str(I(k)-2) C(I(k)-2) int2str(RO(I(k)-2))],'color','r');
end;text(Ra(f+2), xbl1(Ra(f+2)) , [int2str(f+2) Ca(f+2) int2str(Ra(f+2))] , 'color','r' );
    text( Ra(f-2), xbl1(Ra(f-2 )), [ int2str(f-2) Ca(f-2) int2str(Ra( f-2))],'color','r' );
end; if ~Bthesis, if  Q(f),  text( Q(f), xbl1(Q(f )) ,[ 'Q' ] , 'color', 'b' );
end; text(Tp(f), xbl1(Tp(f)),'Tp' ,'color','b' ); 
if (~isempty(S)), text(S(f),xbl1(S(f )),'S', 'color', cb);
end; end; if ~Bthesis, if 1, text(Pb2O(f),xbl1(Pb2O(f)),[ 'PbO'] ,'color','r' ,'FontSize',14);
 if ( Pe2fO(f) ), text(Pe2fO(f ) , xbl1( Pe2fO(f) ),[ 'PeO'] ,'color', 'b', 'FontSize',14);
end;end; if (Tb1fO(f)),text(Tb1fO(f) , xbl1(Tb1fO(f) ), [ 'TbO'], 'color', 'b','FontSize',14);
 end; if (Tef1O(f) ),text(Tef1O(f), xbl1(Tef1O(f )), [  'Te' ] ,'color', 'b' , 'FontSize', 14); 
 end;   end;  end; if  exist('xbl2', 'var'),,  m=m+1;   subplot( N,1,m);
plot( U : V , xbl2( U : V) ,  cyan  , 'LineWidth', 2 );  xlim([ U   V ]) ; 
 j =1; te = Tef1( f ) ;  tb = Tb1f( f )  ;
%[ slmaxc{f} ,  slmaxic{f} ]= max(slopes(Limit ,Pb2(f+1)- 1, xbl1) );
if BL,  line( [ U, V] , [0, 0] , 'Marker', '.'  , 'Color', 'k' ); 
elseif Afilemit ==213 ,   line( [ U, V] , [0, 0] , 'Marker', '.'  , 'Color', 'k' );   
  elseif 1,  line([ U, V], [ 0,0 ] , 'Marker' , '.', 'Color', 'k' , 'LineWidth', 1);    
end;  set( gca, 'Units' , 'normalized', 'FontUnits','points', ... 
 'XTick', U:fix((V-U)/2):V,  'FontUnits', 'points', ......
 'FontWeight', 'normal', 'FontSize', fonts16, 'FontName', 'Times');
 if Afilemit ==209 || Afilemit ==210 , repm=0.02; repM=0.02; 
 minxbf2uv=min( [xbl2( U: V)])-0.02 ; maxxbf2uv=max([xbl2(U: V)])+0.02;
else repM=0.1;repm=0.1;
    minxbf2uv=min([xbl2(U:V)])-0.1;maxxbf2uv=max([xbl2(U:V)])+0.1;
 end;  set(gca,'XTick', U:fix((V-U)/2):V ); set(gca, 'XTickLabel', (get(gca, 'XTick')));
CELL3y = minxbf2uv : (maxxbf2uv- minxbf2uv)/2 : maxxbf2uv;
for cel=1: length(CELL3y) [ST1adc{cel,1}, cel2]=sprintf('%5.2f',CELL3y(cel)); end; 
 set(gca,  'YTick', CELL3y); set(gca,    'YTickLabel', ST1adc );  
title(['Channel 2 ' lead2s ' of  class '  Ca(f)  ', Beat ' int2str(f) ', Record ' Afilemits] ...
,'color', 'r' ,'FontSize' ,16); CL60= int2str(Pb2O(f)) ;ylim([ minxbf2uv maxxbf2uv ]); 
 zerrn2fm1= zerrw2(f-1);zerrn2f= zerrf2(f );zerrn2fp1= zerrA2(f+1);    
 y2= minxbf2uv+repm ; y2m= minxbf2uv+repm ; y2M= maxxbf2uv-repM ;   
xbl2Rafrm =xbl2(Ra( f-1));xbl2Rafr =xbl2(Ra( f ));  xbl2Rafrp =xbl2(Ra(f+1 ));
%   if track&&DBUG dbstop 2513; end; 
  if Bthesis, if  bon,  if (( K2-1)> 0), if  (Pb2( f-1)>U) ,  
 text( Pb2(f-1 ) , xbl2( Pb2( f-1) ), [   'Pb' ] , 'color', 'r' , 'FontSize' ,15 );
end; if (Pe2f(f-1)>U),text( Pe2f(f-1), xbl2(Pe2f(f-1)), [ 'Pe'] ,'color', 'r' , 'FontSize' ,15 );
end; if (Tb1f(K2-1)>0), 
    text(Tb1f(K2-1), xbl2(Tb1f(K2-1)),['Tb'] ,'color','r', 'FontSize',14);
end; if (Tef2(K2-1)>0), 
    text(Tef2(K2-1), xbl2(Tef2( K2-1)),['Te'] ,'color','r','FontSize',14);
end; end;  if ( Pe2fO(f)-Pb2O(f)>20)  if ( Pb2O(f)>0), 
    text(Pb2O(f),xbl2(Pb2O(f)) ,[ 'Pb' ] , 'color', cb , 'FontSize',14);
end; if (Pe2fO(f)>0), text(Pe2fO(f ) , xbl2( Pe2fO(f) ),[ 'Pe' ] ,'color', 'b' , 'FontSize',14);
end;end; end; if bon, text(Tb1fO( f) , xbl2( Tb1fO(f ) ), [ 'Tb'] , 'color','b', 'FontSize',14);
 if 1, text(Tef2(f), xbl2(Tef2(f )), [    'Te' ] ,'color', 'b' , 'FontSize' ,15);
 end;  text( Pb2( f +1) ,  xbl2( Pb2( f +1) ),  [ 'Pb' ] , 'color', cr, 'FontSize', 14);
    text( Pe2f( f  +1) ,  xbl2( Pe2f( f +1) ),  [ 'Pe'  ] , 'color', cr, 'FontSize', 14 );
end;  if ~Bthesis, text(Tb1fO( f ),xbl2(Tb1fO(f) ), ['NANTbM' ] , 'color', 'b'  );
 text( Tef1O( f ), xbl2( Tef1O( f) ), [   'NANTeM'    ] , 'color', 'b' );
  text( Pb2( f  +1) ,  xbl2( Pb2( f +1) ),  [ 'NANPbM' ] , 'color', cr );
   text( Pe2f( f  +1) ,  xbl1( Pe2f( f +1) ),  [ 'NANPeM'  ] , 'color', cr );
end; if DBUG&&0, if I(k)+1<=length(RO), text( RO(I(k)+1), xbl1(RO(I(k)+1))-0 ...
, ['R' int2str(I(k)+1) C(I(k)+1) int2str(RO(I(k)+1))],'color',cr,  'FontSize' ,14);
end;  if  I(k)-1 >=1, text(RO(I(k)-1),xbl1(RO(I(k)-1))-0.0 ,   ...
  [ 'R' int2str(I(k)-1) C(I(k)-1) int2str(RO(I(k)-1))],'color','r' ,  'FontSize' ,14 );
 end;  end; end; end;  if  N>2 && exist('xbl3', 'var'),  m=m+1; 
subplot(N,1,m );  xdfcn = [ U:fix((V-U)/2):V ] ;
  plot( U : V , xbl3( U : V) , 'g'  , 'LineWidth', 2 ); hold on; 
 set(gca,  'Units','normalized',   'FontUnits','points', ...  
 'FontWeight',  'normal', 'FontSize', fonts16, 'FontName','Times');
maxxbl1= max( xbl3( U : V ))+0.1 ; minxbl1=min( xbl3( U : V))-0.1 ;  
fy25= ( minxbl1:(maxxbl1- minxbl1)/2: maxxbl1) ; 
set(gca, 'YTick', fy25) ;   xlim([ U  V ]);ylim( [ minxbl1 maxxbl1 ]); grid off;
set(gca, 'XTick', U:fix((V-U)/2):V  ) ; set(gca,  'XTickLabel', xdfcn ) ; 
if BL,   line( [ U, V] , [0, 0] , 'Marker', '.'  , 'Color', 'k' );   
elseif 1,  line([ U, V], [ 0,0 ] , 'Marker' , '.', 'Color', 'k' , 'LineWidth', 1);     
end; for fy=1: length( fy25 ) [ STxbf3{ fy,1}, ersg] = sprintf('%.2f',fy25(fy)); end; 
 set( gca,  'YTickLabel',  STxbf3  )  ; iqrs= { 'FontSize', 14 } ;
title([ ' Channel 1 + Channel 2 ' ' of Beat '  int2str(f)  ', Record  ' Afilemits ]   ...
 ,'color', 'r' ,  'FontSize' ,16); if  DBUG&&0,%h_940s(i) = figure( h_940(i))
text(Ra(f-1),xbl3(Ra(f-1)), [int2str(f-1) Ca(f-1) int2str(Ra(f-1))],'color','r','FontSize',14);
text(Ra(f ),xbl3(Ra(f) ), [ int2str(f ) Ca(f )  int2str(Ra(f )) ] ,'color','b' , 'FontSize',14);
text(Ra(f+1),xbl3(Ra(f+1)), [int2str(f+1) Ca(f+1) int2str(Ra(f+1))],...
    'color','r','FontSize',14);
end;  if (0), text(S(f),xbl3(S(f )) ,'S' , 'color', 'r'  ); 
if exist('Rp','var')&&(Rp(f )>0)&& (~isempty(Rp)),text(Rp(f), xbl3(Rp(f )),'rp', 'color','r'); 
end;  end; if Bthesis, if ( Pb2O(f-1)> 0) &&Pb2O( f-1)>U , 
 text( Pb2O( f-1), xbl3(Pb2O( f-1) ),  [  'Pb' ] , 'color', cr , 'FontSize', 14 ); 
end; if (Pe2fO(f-1)>U), text(Pe2fO(f-1), xbl3(Pe2fO(f-1)),[ 'Pe'] ,...
        'color', cr,'FontSize',14);
end; if (Tb1fO(K2-1)>0),  A3= [ int2str(( K2-1))  'Te' ] ; 
    text(Tb1fO( K2-1) , xbl3( Tb1fO( K2-1 ) ), [ 'Tb' ] , 'color', cr , 'FontSize', 14 );
end; if (Tef1O(K2-1)>0), 
    text(Tef3(K2-1), xbl3(Tef3(K2-1)),['Te'],'color',cr,'FontSize',14);
 end; if (Pb2O(f)>0), text( Pb2O( f) , xbl3(Pb2O( f) ), [ 'Pb'] , 'color', 'b' , 'FontSize',14); 
end; if (Pe2fO(f )>0),  text(Pe2fO( f) , xbl3( Pe2fO(f)),[ 'Pe'] ,'color', 'b' , 'FontSize', 14);
end; if (Tb1fO(K2)>0), text(Tb1fO(K2), xbl3(Tb1fO(K2)),['Tb'], 'color','b' ,'FontSize',14);
end; if (Tef3(K2)>0), text(Tef3(K2 ), xbl3(Tef3(K2)) , ['Te' ] ,'color',cb, 'FontSize',14);
end; if (Pb2O(f+1)<V), text(Pb2O(f+1),xbl3(Pb2O(f+1)) ,['Pb'], 'color','r', 'FontSize',14); 
end; if (Pe2fO(f+1)<V),text(Pe2fO(f+1), xbl3(Pe2fO(f+1)),['Pe'],'color',cr,'FontSize',14);
end;  end; end;  ROM=[d(1:2),d(4:6),d(10:11),d(13:14),d(16:17)]; 
FIGFILE=[ Ca(f ) 'Reprod' Afilemits B int2str(f) time  int2str(R1000(i))  dates , '.png' ]; 
FIGUFILE = fullfile(PATFIG, FIGFILE); if 1&DBUG dbstop 2605; dbstop 2608; 
saveas( h_940(i) , [FIGUFILE  ] );
 %  saveas( h_940(i) , [FIGUFILE   ], 'jpg' )
 end; end;end;  if (~isempty(zlTwM) ||~isempty( zlPw)),
if  length( zlTwM) >= 2, Bzero=2; BzeroF =  zlTwM  ; bz=1; 
elseif   length( zlPw)>=2, Bzero=2 ; BzeroF =zlPw; bz=0;
else  Bzero= length(zlPw); BzeroF =zlPw; bz=0;
end;  filter_param = 16; thresh_param= 2;  pb_param = 0.1;
 for  k = 1  :  Bzero    if 1, K2 =  BzeroF( k)  ; 
   f= K2;    if  K2==1, continue; end; if (f+1)>= length(Ra), continue; end; 
  Rg= Ra(f) ; [ I980  rov ] = find (RO == Rg) ; zlwC(k)= C(I980 );
 A1B=   [ 'zero3000' Afilemits B  Ca(f) int2str( f)  int2str( Rg) ];
 if Afilemit == 208&&  f== 2292, continue ; end;
h1011(k) = figure('Name', [ ' 2625' A1B ], ...
'Position', [ 10 40 700 500]); U=Ra(f-1)-90;V=Ra(f+1)+90; 
h = gcf ;  set(h,'WindowStyle','docked'); 
if  (U <=0),U= 1; end; if (V >NS),V = NS-1; end;
end; if 10, subplot(2,1,1); plot( U : V , xbl1( U : V) , cyan , 'LineWidth', 2 ); 
 if BL, line([ U, V], [ 0,0 ] , 'Marker' , '.', 'Color', 'k' , 'LineWidth', 1);  
 elseif 1,  line([ U, V], [ 0,0 ] , 'Marker' , '.', 'Color', 'k' , 'LineWidth', 1); 
 end; set(gca,'Units', 'normalized',   'FontUnits', 'points',  ...
 'FontWeight', 'normal',  'FontSize', 16 , 'FontName', 'Times' ) ; 
 if Afilemit ==203, minxdfc = min(xbl1(U:V) )-0.1; maxxdfc =max(xbl1(U:V) )+0.1;  
 else   minxdfc = min(xbl1(U:V) )-0.2 ; maxxdfc =max(xbl1(U:V) )+0.2 ;  
end; fyxdfc=  [   minxdfc   0  maxxdfc ] ; set( gca,  'YTick', fyxdfc  ) ; 
 for i= 1: length( fyxdfc ) [ STfyxdfc{ i,1} , errm] = sprintf('%.2f', fyxdfc(i) ); end; 
  set( gca,  'YTickLabel',  STfyxdfc ) ;  ylim([ minxdfc maxxdfc  ]) ; xlim([U  V]) ;   
A3=[ U  floor((V+ U-1)/2)  V ];set( gca,'XTick', A3);set(gca,'XTickLabel', A3 ) ; 
title(['Channel 1 ' lead1s ' of  class ' Ca(f ) ', Beat '  int2str(f) ', Record '  Afilemits]...
,'color', 'r' ,  'FontSize' ,16); rafms= int2str(Ra(f-1));rafps=int2str(Ra(f+1)) ; 
 if Afilemit==113 , zerr43fm1= zerrA1(f-1); zerr43f= zerrw1(f);zerr43fp1=zerrf1(f+1) ; 
end; xbl1Rafzm= xbl1(Ra(f-1)) ; xbl1Rafz = xbl1(Ra(f)); xbl1Rafzp=xbl1(Ra(f+1)); 
  if DBUG&BL,  dbstop 2640; end; 
 if (Afilemit== 201  &&f==221) || (Afilemit== 234 ),
 text(Ra(f-1),xbl1(Ra(f-1)), [ int2str(f-1) Ca(f-1)  ],'color','r' ,'FontSize',14);
 text( Ra(f) ,xbl1(Ra(f )), [ int2str(f)  Ca(f) int2str(Ra(f))] ,'color','b' , 'FontSize', 14);  
text(Ra(f+1)-5,xbl1(Ra(f+1)), [int2str(f+1) Ca(f+1)  ],'color','r', 'FontSize',14); 
 elseif (Afilemit== 208  &&f==2 )       
    text(Ra(f-1),xbl1Rafzm, [ int2str(f-1) Ca(f-1)  ],'color','r' ,'FontSize',14);
 text( Ra(f) ,xbl1Rafz , [ int2str(f)  Ca(f) int2str(Ra(f))] ,'color','b' , 'FontSize', 14);  
text(Ra(f+1)-5,xbl1Rafzp , [int2str(f+1) Ca(f+1)  ],'color','r', 'FontSize',14);   
 else  text(Ra(f-1),xbl1(Ra(f-1)), [ int2str(f-1) Ca(f-1)  ],'color','r' ,'FontSize',14);
 text( Ra(f) ,xbl1(Ra(f )), [ int2str(f)  Ca(f) int2str(Ra(f))] ,'color','b' , 'FontSize', 14);  
text(Ra(f+1)-5,xbl1(Ra(f+1)), [int2str(f+1) Ca(f+1)  ],'color','r', 'FontSize',14); 
     
 end;  if Pe2f(f)- Pb2( f )>20   if ( Pb2(f )>0  ), %bthesis 
text( Pb2( f ) , xbl1( Pb2(f) ), [   'Pb' ] , 'color', 'b' ,  'FontSize', 14 );
end; if (Pe2f(f)>0),  text(Pe2f(f), xbl1( Pe2f(f)),['Pe' ] ,'color', cb , 'FontSize', 14 );  
end; end;   if  (Tef1(K2)-Tb1f(K2))>360,    if  (Tb1f(K2)> 0) ,
 text(Tb1f(K2),xbl1(Tb1f( K2)),[ 'Tb'  ], 'color', 'b' , 'FontSize', 14);
end; if (Tef1(K2)>0), text(Tef1(K2), xbl1(Tef1( K2)), [ 'Te'] ,'color','b', 'FontSize',14); 
end; if 0, text( Q(f) , xbl1(Q(f )) , 'Q' );  text( Tp(f ) ,  xbl1( Tp(f ) ),'Tp' ); 
   text(S(f ) , xbl1(S(f )),'S' );   text(Rp(f ), xbl1(Rp(f )), 'rp' );  
 end;  end;    if Pe2f(f+1)- Pb2( f+1 )>350
 if  Pe2f(f+1) , text( Pe2f(f+1), xbl1( Pe2f(f+1 )),[  'Pe' ] , 'color', cr , 'FontSize',14);  
end; if  Pb2(f+1), text( Pb2(f+1),xbl1( Pb2(f+1)),[ 'Pb'] , 'color', 'b', 'FontSize', 14);
end;  end;end;  if 10, if 10, subplot(2,1,2);
% if Afilemit ==118, xbl2=xbl2+0.2; end; %thesis 
plot(U : V, xbl2( U : V) , 'm'  , 'LineWidth', 2 ); fm1s= int2str(Ra(f-1))
 if BL, line([ U, V], [ 0,0 ] , 'Marker' , '.', 'Color', 'k' , 'LineWidth', 1);  
 elseif 1,  line([ U, V], [ 0,0 ] , 'Marker' , '.', 'Color', 'k' , 'LineWidth', 1); 
 end; set(gca,'Units', 'normalized' ,  'FontUnits', 'points',  ...
 'FontWeight', 'normal',  'FontSize', 16 , 'FontName', 'Times' ) ; 
set(gca,  'XTickLabel', (U:fix((V-U)/2):V) ) ;   xlim([U  V]) ;   
if Afilemit ==221,minxbl2= min(xbl2(U:V))-0.12;maxxbl2=max(xbl2(U:V))+0.12;  
elseif Afilemit==201, minxbl2=min(xbl2(U:V))-0.10;maxxbl2=max(xbl2(U:V))+0.10;  
else  minxbl2  = min(xbl2(U:V))-0.1;  maxxbl2  =max(xbl2(U:V) )+0.1;  
 end; fyxbl2=  [ minxbl2   0  maxxbl2 ] ; set( gca,  'YTick', fyxbl2 ) ; 
 for i= 1: length( fyxbl2 ) [ STfyxbl2{ i,1} , err] = sprintf('%.2f', fyxbl2(i));  end;
 set( gca, 'YTickLabel',  STfyxbl2) ;ylim([ minxbl2 maxxbl2 ]) ; xlim([U  V]) ;   
A3=[ U  floor((V+ U-1)/2)  V]; set( gca,'XTick', A3); set(gca,'XTickLabel', A3) ; 
title([ 'Channel  2 ' lead2s  ' of  class ' Ca(f) ', Beat ' int2str(f) ' Record ' ...
Afilemits],  'color','r', 'FontSize' ,16);  
if Afilemit ==223, zerrz2fm1=zerrw2(f-1);zerrz2f= zerrf2(f );zerrz2fp1= zerrA2(f+1);
elseif Afilemit ==207&&f==1655 , 
    zerrz2fm1= zerrf2(f-1);zerrz2f= zerrf2(f );zerrz2fp1= zerrf2(f+1);
elseif (Afilemit ==207 &&f==1655) , 
    zerrz2fm1= zerrf2(f-1);zerrz2f= zerrf2(f);zerrz2fp1= zerrf2(f+1);
else zerrz2fm1= zerrw2(f-1);zerrz2f= zerrf2(f );zerrz2fp1= zerrA2(f+1);    
end;  if DBUG&&BL, dbstop 2687; end;
end; if (Afilemit== 201  &&f==221)  ,   
text(Ra(f-1),xbl2(Ra(f-1))+zerrz2fm1, [int2str(f-1) Ca(f-1)], 'color', cr,'FontSize',15);
text( Ra(f ) , xbl2(Ra( f )) +zerrz2f, [ int2str(f)  Ca(f)  ],'color','b' ,'FontSize',15 );
text(Ra(f+1)-3,xbl2(Ra(f+1))+zerrz2fp1, [int2str(f+1) Ca(f+1)],'color',cr,'FontSize',15);
elseif 0,     text(Ra(f-1), xbl2(Ra(f-1))+0 , [int2str(f-1) Ca(f-1)], 'color', cr,'FontSize',15);
text( Ra(f ) , xbl2(Ra( f )) +0, [ int2str(f)  Ca(f)  ],'color','b' ,'FontSize',15 );
 text(Ra(f+1)-3  , xbl2(Ra(f+1)) +0 , [ int2str(f+1) Ca(f+1) ] ,...
'color', cr,'FontSize',15 );    
else 
end ; if Pb2(f )&&0, text(Pb2(f ),xbl2(Pb2(f)) , [ 'Pb'], 'color','r' ,'FontSize', 15  ) ;
end; if 0, text( Tb1fO( f ) ,xbl2( Tb1fO( f) ),  [ 'Tb' ] , 'color', 'b','FontSize',15 );
    text( Tef1O( f ) , xbl2( Tef1O( f) ),[   'Te'    ] , 'color', 'b','FontSize',15 );
end;if 0,text(RO(I980-1),xbl1(RO(I980-1)),[C(I980-1) int2str(RO(I980-1))],'color','b');
text( RO(I980+1),xbl1(RO(I980+1)),[ C(I980+1) int2str(RO(I980+1))  ] ,'color','b'  );
 end; if  0&&exist('Tb1fM', 'var')  &( K2< length(Tb1fM)) &~isnan(Tb1fM(K2) )
text( Tb1fM( K2) , xbl2( Tb1fM( K2) ),[  'TbM'   ] , 'color', 'b' ); %size
end; if 0&&exist('Tef1M','var') &&( f <=length(Tef1M)) &&(~isnan(Tef1M( f)))
    text( Tef1M( f ) , xbl2( Tef1M( f) ), [   'TeM'  ] , 'color', 'b' ,'FontSize',15);
end;  first_max=xbl1(Ra(f))-xbl1(Q(f));
if  (f+1)< length( Ra),  if 0&Pe2f(f+1), 
 text(Pe2f(f+1), xbl2( Pe2f(f+1)),[ 'Pe' ] ,'color',cr,'FontSize', 15);
end; if  0,  text( Pb2(f+1) , xbl2( Pb2(f+1)),[ 'Pb' ]  ,'FontSize',15,'color', cr );   
end;  maxi=maxi+((first_max-maxi)/ filter_param);             
slope_thresh= (thresh_param /16)*maxi; pt_thresh= pb_param*slope_thresh; 
RRNdur2(f) = Ra(f+1) - Ra(f) ;  Ritmin2(f) = 0.111 * RRNdur2(f) ; 
Ritmax2(f) = 0.683 * RRNdur2(f) ;   Te2range2(k) = fix( Ritmax2(f) ) ; 
  j=1; Te2S(k)=   Ra(f+1) ; Te2E(k)= Ra(f) +Te2range2(k);    
for i= Te2S(k) : -1 : Te2E(k)    
 if (abs(slope(i,xbl1))>pt_thresh)&& (abs(slope(i-1,xbl1))>pt_thresh) ...
   %&& (slope(i+2)> pt_thresh)
TE2(f,j) =i; j=j+1;      end;  end;
if  exist('TE2(f,1)' ,'var'),TE2f2(f ) = TE2(f,1);  end;  
if bz, if k<= length( zlTwM), 
 if  j-1>0,TE2f(f)=TE2(f,j-1); Te2fzlTw(k)=TE2f(f);    
TEW( (k)) = TE2f( zlTwM(k) )-Tb1f( zlTwM( k) ); Tef1(f)=TE2f( f );   
 else  end;  end;  te = Tef1( f );  tb = Tb1f(f ); 
if  exist('TE2f', 'var'), TE2f= double(TE2f); end;
 %  if (Tef1(f) >0) text( Tef1(f ),  xbl1(Tef1( f )),'Tef1','color','r');end;
  if 0 &&( (Tef1( f ) -Tb1f(f)) < 0 )  ,
 fprintf(' 2720  f=%d%cTb1f=%d Tef1(K2)=%d%dPb2(f+1)=%dPe2f=%d\n',...
f, Ca(f), Tb1f( f ) , Tef1( f ), TE2f(K2), Pb2(f+1), Pe2f( f+1) ); %          -     
 text( TE2f(f ) ,xbl1(TE2f( f )),[ int2str( ( f ) ) 'Te2f' ],'color','r'); 
end;end; end;  end;  hold off;dates 
FIGFILE=[  A1B Afilemits int2str(f)    B dates, '.png' ]; 
FIGUFIL=fullfile(PATFIG, FIGFILE) ;  
    saveas( h1011(k) , [  FIGUFIL  ]) ;if 0&DBUG, dbstop 2732; 
fprintf(' 2731  Pause... Testing T-wave Press any key \n');  
end;end; end; if 1, fprintf(' ST 2730 \n'  ); zzlenC= Ca(zzlen')' 
  if exist( 'hrxdiff', 'var' ),hadcgains= close( hrxdiff ) ;clear hrxdiff; end; 
%if exist( 'h_9_PT', 'var' ), hl9s = close( h_9_PT ) ; end; 
if 0, if exist( 'h2AFL', 'var' ), h2AFLs=close( h2AFL);  end; end; 
%hband1s=close(hband1);clear hband1;%hband1s=close(hband2);clear hband2;
 if exist( 'hTOFF2', 'var' ), hTOFF2s= close(hTOFF2) ; clear hTOFF2;end; 
  if exist( 'hTOFF3', 'var' ), hTOFF3s=close( hTOFF3);clear hTOFF3; end; 
h2s =close( hadc ) ; clear hadc; AXQRS={}; AXQRSYM={}; 
 clear  xbl3 xr   iqrs ;   clear S1 S2 SS TB TE Tb Te1 TE2v ; 
 [ zlTw1262]= find((Tef1(1:lTw)-Tb1f(1:lTw)) <=0);AFLAGT =0;
x = xbl1; if size(xbl1,2) ==1, xbl1= xbl1'; end;
mf= 1; Mf= 100; fM = 100; tM = 201;  
 AXQRSYM{1}= [xbl1(1: Ra(1)+100)]' ; AXQRSTF{1} = [ xbl1(Q(1) : S(1))] ;
AXQRST={};  AXQRSTF={}; A2=0; 
if Afilemit == 207, AA = setdiff([  AA 1647]  , [ 1653  ]')';          
  end;  [ AAS AASI ] =sort( AA ,'ascend') ; %dbstop 2770; 
end; end; fprintf(' ST 2750 \n'); for  K = 1 : RL    if 1, 
 I =find(Ra(K)== RO);
 A1TMA1std(K) =0 ;   A1TMA1mean(K)= 0 ; A1TMA1min(K)=0 ;
 A1FMA2std(K) =0 ;   A1FMA2mean(K)=0 ; A1FMA2min(K ) = 0 ;
A1FSTDstd(K) = 0 ;   A1FSTDmean(K)= 0 ;  A1FSTDmin(K) = 0 ;
FFFQRSD3(K)=0; A1F1S(K)=0; A1F2S(K)=0; if Pe2f(K)<=0,Pe2f(K)=Q(K);end; 
if Pb2(K)<=0, Pb2(K)= Pe2f(K); end;  if Tb1f(K)<=0,Tb1f(K) = S(K); end; 
 if   Tef1(K) <=0, Tef1(K ) = Tb1f(K) ; AFLAGT =  AFLAGT+1;  end; 
AXQRSTF{K} =  [ xbl1( Q(K) : S(K ) ) ] ; len = length( AXQRSTF{K});  
 if  len ==0,AXQRSTF{K} =  [  [ xbl1( S(K) : Q(K ) ) ]  ] ;  end; 
 len = length( AXQRSTF{K} );   NFFT = 2^nextpow2(len); 
YY2=[zeros(1, NFFT- len)]; YY=[ AXQRSTF{K}, YY2]; Y= fft(YY, NFFT/2); 
Pyy = 10*log10(Y.* conj(Y) / length(Y));  FFFQRSD3( K ) = max( Pyy) ;   
if 0, AXQRST{K} = [ [ xbl1( Pb2( K ) : Tef1( K) )]' ] ; end;
 % [st ,  t ,f  ] = st(timeseries,minfreq,maxfreq,samplingrate,freqsamplingrate) 
if  (Ra(K)+100 <= NS ) && ( Ra(K)-100 >1 ),
AXQRSYM{K} =  [  [ xbl1( Ra( K)-100 : Ra( K)+100 )]'  ] ;        
else   err1=  find( Ca(K) == Ca) ;  if  length(err1) >1 ,err1rep=err1(2); end;
  if  Afilemit == 207 && K==1 , err1rep=236;
AXQRSYM{K} = [ [ xbl1( Ra( err1rep)-100: Ra( err1rep)+100)]' ];%break;
else  AXQRSYM{K} = [ [ xbl1( Ra( err1rep)-100 : Ra( err1rep)+100 )]' ];
end;end; [  s3 s3i ] =  find( AAS == K ) ; %  dbstop 2765; 
end; if  BL  || 1 ,  if 1,  %  1 is BL 
  [ z , tst , fst ] = st(  AXQRSYM{K} , mf  , Mf  ,1 ,1 ) ;  
A1ST = abs(z);     [ fM tM ]= size(A1ST) ;  
for t= 1: tM   [ A1TMA1(K ,t)  F3( K , t)  ] = max(A1ST(: ,t ) ) ;  end;            
 for f= 1: fM    [  A1FMA2(K,f) F4(K,f)  ] = max(A1ST(f ,:) ) ;  end; 
 for f= 1: fM    [   A1FSTD(K,f)   ] =  std(A1ST(f ,:) ) ;      end;  
if  ~rem( K,500) , fprintf(' ST  2770   %d \n', K );  end;   
if 0,A1STa{K} =A1ST; [ A1F1(K,:)] =(A1ST(: , tM)); [A1F2(K,:)]=(A1ST( fM,:));
 for f= 1: fM      [  A1F1S(K)  ] =  A1F1S(K) + A1F1(K,f)  ;    end;
 for t= 1: tM     [  A1F2S(K )  ] =  A1F2S(K) + A1F2(K ,t)  ;  end;
end;   A1TMA1std(K ) = std(A1TMA1(K,: )) ;  
A1TMA1mean(K)= mean(A1TMA1(K,:)); A1TMA1min(K)= min(A1TMA1(K,:));
 A1FMA2std(K)= std(A1FMA2(K,:)); A1FMA2mean(K) =mean(A1FMA2(K,: ));
 A1FMA2min(K ) = min(A1FMA2(K ,: )); A1FSTDstd(K ) = std(A1FSTD(K,: )) ;
 A1FSTDmean(K)= mean(A1FSTD(K,: ));A1FSTDmin(K) =min(A1FSTD(K,:));
end; elseif    isequal( B,'BLN'), if   s3 ,   %thesis 
 [ z , tst , fst ] = st(  AXQRSYM{ K} , mf  , Mf  ,1,1  ) ;  
A1ST = abs(z);     [ fM tM ]= size(A1ST) ;  clear tst fst ; 
for t= 1: tM   [ A1TMA1(K,t)  F3( K, t)  ] = max(A1ST(: ,t ) ) ;   end;            
 for fr= 1: fM   [   A1FMA2(K,fr) F4(K,fr)  ] = max(A1ST(fr ,:) ) ;  end; 
for fr= 1: fM    [   A1FSTD(K,fr)   ] =  std(A1ST(fr ,:) ) ;    end;  
 if 1, fprintf(' ST 2790   %d \n',  K );  end;   
end; end;    if   s3 ,  if 1,  s1 = 5 ;   n = 1; f = K ; 
 if Afilemit ==234,dTBE2= 0.2;
 else dTBE2= 0.2;
 end; if (K)>= length(Ra), continue; end; clear tst fst ; 
MnanN = [ 'ECG ' CLASS{ca(K) } ' MIT-BIH ' Ca(K) ...
' Record ' Afilemits ' Rpeak ' int2str(Ra(K))  ' Beat ' int2str(f) ] ; 
 PbE=[ Ca(K)  ' Record ' Afilemits ' Rindex ' int2str(Ra(K)) ' Beat ' int2str(f)] ; 
minAXQRSYM=min(AXQRSYM{f})-dTBE2; maxaxqrsym=max(AXQRSYM{f})+dTBE2 ; 
celstr1= minAXQRSYM: (maxaxqrsym-minAXQRSYM)/2: maxaxqrsym;
cnptoqc=[ CLAS{ ca(K)} Ca(K)   'Rec' Afilemits 'Beat' int2str(f) ] ;  cak =ca(K); 
cnptocn =[ 'class '  Ca(K) ', Rec  ' Afilemits ', Beat  ' int2str(f) ]  
AAflagn= ~isequal(Ca(K),'N') & ~isequal(Ca(K),'Q'); 
if  cak == 1 ||  cak == 2  ||  cak == 3  ||  cak == 11 ||  cak == 34 , col='g' ;  
 elseif  cak == 4  || cak == 7 || cak == 8  || cak == 9,  col='m'; 
elseif  cak == 5 || cak == 10, col='r'; elseif  cak ==6,col=cb; else   col=ck; 
 end; if 1, PbE= [  ' Class '  Ca(K)   ', Beat  ' int2str(f)  ', Record  ' Afilemits] ; 
 else PbE= [ 'Rec  ' Afilemits ' Rindex  ' int2str(Ra(K))  ' Class '  Ca(K)] ;
end; pos(s3i,:) = [ (200+K)/10  (300+K)/10 screen(3)-200  screen(4)-100 ];
end; if AAflagn, 
if 1, if Bsub,  h2265(s3i) =figure( 'Name', [Afilemits ' 2820 ST '  ...
cnptoqc],'Position', [K/10 30 screen(3)/2 800]); h=gcf;set(h,'WindowStyle','docked');  
 Cf =subplot( s1,n ,1);  title( Cf , [ '1.' cnptocn] ,'color', 'r', 'FontSize',16); 
  set(gca,  'Units', 'normalized', 'FontUnits', 'points', ...
 'FontWeight','normal',  'FontSize', fonts16 , 'FontName','Times'   ) ;
plot( AXQRSYM{f } , col  , 'LineWidth', 2) ;  XTick=  [1:(tM-1)/2 : tM]  ;
set(gca, 'XTick', 1: (tM-1)/2 : tM ); set(gca,'XTickLabel', [1:(tM-1)/2 : tM] ); 
title( [ cnptocn ] ,'color', 'r','FontSize' ,16 );
%cnptocn=['' CLASS{ca(K)} ' MITBIH ' Ca(K) ' Rec' Afilemits 'Beat' int2str(f)] ; 
ytick=( minAXQRSYM:(maxaxqrsym- minAXQRSYM)/2:maxaxqrsym) ;
for i= 1: length( ytick ) [ ST1{ i,1} , errsg] = sprintf('%.2f',  ytick(i) ); end;
  set(gca, 'YTick',celstr1 ) ; set(gca, 'YTickLabel',  ST1 ) ; 
 ylim([minAXQRSYM  maxaxqrsym]); xlim([0  201]) ;
 zerrw2=['Record ' Afilemits ', Beat ' int2str(f) Ca(K)];
 title( [  cnptocn ] ,'color', 'r',  'FontSize',16 );    
ylabel('Amplitude, mV','Color',ck,'FontSize',16);xlabel('Time','Color',ck,'FontSize',16); 
end;end;  if 1, if Bsub,   Cfj = subplot(s1 , n ,  2); 
title(Cfj, ['2. Time-Freq Contour of ST matrix 201x100'] ...
 ,'color', 'r' ,'FontSize',16);dbstop  2856;  
elseif ~Bsub h2830tfc(s3i)= figure( 'Name',[ '2830 TFC ' cnptoqc ],...
'Position',   [  K/10   30  screen(3)/2 screen(4)-200]  );
title(['  Time-Frequency Contour of  S-transform matrix 201x100' ] ...
,'Color', 'r' ,'FontSize' ,16);   h=gcf;set(h,'WindowStyle','docked') ;      
end; if  Afilemit== 222 threhM= 0.01; threhm= 0.01; 
else  threhM= 0.01; threhm= 0.01; 
end; [ min1651 min1651I]= min(A1ST); [min1653 min1653I]=min(min1651-threhm);
[max1651 max1651I]=max(A1ST); [maxA1STp maxA1STpI]=max(max1651+threhM);
 set(gca, 'Units', 'normalized',  'FontUnits', 'points'  ...
 , 'FontWeight','normal',  'FontSize', fonts16, 'FontName','Times' ) ;
if 10, xlim([0  tM]) ;
hold on; plot( A1ST' , 'LineWidth', 2,'Color', col ); xbl3= [1:(tM- 1)/2 : tM];
xlabel(' Time ' ,'FontSize',16,'Color' , cr);ylabel('Freq' , 'FontSize',16, 'Color', cr);
else xlim([ 0   fM]) ;% axis([ 0, tM,  min1653,  maxA1STp]) ;   
hold on; plot( A1ST  , 'LineWidth', 2 ,'Color', col );  xbl3= [0:(fM+0)/2 : fM ] ;
xlabel(' Freq ' ,'FontSize',16,'Color' , cr);ylabel('Time' , 'FontSize',16, 'Color', cr);
end; xr = (  min1653:( maxA1STp- min1653)/2 : maxA1STp ); 
set(gca, 'XTick', xbl3) ;  set(gca,'XTickLabel',  [xbl3]);
for i= 1: length( xr ) [ ST2{ i,1} , errsg] = sprintf('%.2f',  xr(i) );  end; 
set(gca, 'YTick', xr) ;  set(gca, 'YTickLabel',ST2); ylim([min1653  maxA1STp]);
da1246= d(1:2),d(4:6),d(10:11),d(13:14),d(16:17)
FIGFtfc= [ 'zST'  'TFC'  Afilemits cnptoqc B da1246 ];
FIGFILEtfc = fullfile(PATFIG, FIGFtfc); 
zerrw2=['Record ' Afilemits ', Beat ' int2str(f) Ca(K)];
title(['Time-Freq  Contour of ST matrix 201x100' ] ,'Color', 'r' ,'FontSize' ,16);     
 if  1&~Bsub dbstop  2856;  
saveas( h2830tfc(s3i), [ FIGFILEtfc  '_' int2str(f)  'png' ], 'png');%thesis 
end;end; if 1,  if Bsub, CfJ  = subplot(  s1 , n, 3);   
title( [ 'C. Time vs Max Amplitude of ST matrix '] ,'color', 'r' , 'FontSize',16);
elseif ~Bsub,h2850tma(s3i)=figure('Name',['2860 tma' Afilemits cnptoqc],'Position', ...
 [ K/10 30 screen(3)/2 screen(4)-200]);h=gcf;set(h,'WindowStyle','docked') ; 
   title( [ 'Time vs MaxAmp of ST matrix ' ] ,'color', 'r' , 'FontSize' ,16 );
  end;   if Afilemit==207, xdfc='%.3f'; x_a= 0.01;
elseif Afilemit==210| Afilemit==100, , xdfc='%.4f'; x_a= 0.001;
  else xdfc='%.3f'; x_a=0.003;   end;
  minA1TMA1f = min(A1TMA1(f, :))-x_a ;maxTMA1f = max( A1TMA1( f,:))+x_a ;
   TeE3 =  ( minA1TMA1f : ( maxTMA1f-minA1TMA1f)/2 : maxTMA1f ) ;
   set(gca,  'Units', 'normalized', 'FontUnits', 'points', ...
 'FontWeight','normal',  'FontSize', fonts16, 'FontName', 'Times'  ) ;
  plot(  A1TMA1( f , : ) , 'LineWidth', 2,'Color', col ); xlim([1  tM]) ; 
TeE3p=[1:(tM-1)/2: tM];set(gca, 'XTick',TeE3p);set(gca,'XTickLabel', TeE3p); 
 for i= 1: length( TeE3 )  [ ST3{ i,1} , errsg]= sprintf( xdfc,TeE3(i) ) ; end; 
set(gca,'YTick', TeE3) ;  set(gca,'YTickLabel', ST3) ;ylim([TeE3(1) TeE3(end)]) ; 
Annottime=[ d(1:2),d(4:6),d(10:11),d(13:14),d(16:17)]
  CA1= [ CLAS{ ca(K) } Ca(K)   'Rec' Afilemits 'Beat' int2str(f) ] ; 
  QM=['Time vs Max amp of STM, Record ' Afilemits  ]
title([QM ', beat ' int2str(f)  ', class ' Ca(K) ],  'color', cr ,'FontSize' ,16); 
cnptocn =['class ' Ca(K) ', Rec ' Afilemits ', Beat ' int2str(f)]
ylabel('Amplitude, mV'  , 'Color', cr ,  'FontSize', fonts16);  
xlabel('201 samples, centered at R' ,'Color', cr, 'FontSize',16);  
FIGFtma=[   'zST' Afilemits 'TMA'   cnptoqc B Annottime int2str(f)  '.png' ];...
FIGFILEtma= fullfile(PATFIG, FIGFtma); if ~Bsub,dbstop  2882; 
saveas(h2850tma(s3i), [ FIGFILEtma   ] );
end; end;  if 1,  if Bsub, RA4 = subplot( s1 ,  n,  4) ; 
 title([ '4.  Freq vs MaximumAmplitude of ST matrix'],'color',cr ,'FontSize',16);
 elseif ~Bsub  h2870fma(s3i) = figure( 'Name',  ['2890  ST ' cnptoqc ] ,     ...
'Position',[K/10 30 screen(3)/2 screen(4)-200]);h=gcf;set(h,'WindowStyle','docked');
title([ ' Freq vs Max Amp  of ST matrix'   ]  ,'Color','r','FontSize' ,16);
end;   plot(   A1FMA2(f ,: )  , 'LineWidth', 2 ,'Color', col ) ;xlim([ 0  fM]);
minA1FMA2f =min( A1FMA2( f, :))-0.01; maxA1FMA2f=max( A1FMA2(f,: ))+0.01 ;
  set(gca,'Units', 'normalized', 'FontUnits', 'points',  ...
 'FontWeight','normal',  'FontSize', fonts16, 'FontName','Times'   ) ;
TeE3= (minA1FMA2f:( maxA1FMA2f-minA1FMA2f)/2: maxA1FMA2f  );
  for i= 1: length( TeE3)  [  ST4{ i,1} , errg] = sprintf('%.2f',  TeE3(i) ) ; end;
 set(gca,'YTick',TeE3); set(gca, 'YTickLabel',ST4); ylim([TeE3(1)   TeE3(end) ]);
 TeE3p=[0:(fM-0)/2: fM];set(gca, 'XTick',TeE3p);set(gca,'XTickLabel', TeE3p); 
ylabel('Max Amplitude','FontSize',16,'Color',cr);
xlabel('Frequency','Color',cr,'FontSize',16);  
zerrw2=['Record ' Afilemits ', Beat ' int2str(f)  ', class ' Ca(K)];
title([ 'Freq  vs Max amp  of ST matrix ' zerrw2], 'Color','r','FontSize',16);
FIGFfma= ['zST' Afilemits 'fma2890' cnptoqc dates B int2str(f) '.png' ];...
FIGFILEfma= fullfile(PATFIG, FIGFfma) ;if ~Bsub,dbstop  2902; 
saveas( h2870fma(s3i), [ FIGFILEfma  ] );
 end; end; if 1, if Bsub,   CflutterJ = subplot( s1 ,  n,  5 ); 
title( CflutterJ , [' 5. Freq vs Std deviation of ST  '] ,'color', 'r' ,'FontSize' ,16 ); 
 elseif ~Bsub h2900fsd(s3i)= figure( 'Name', ['2910 ST ' cnptoqc] ,   ...
 'Position', [K/10 30 scr(3)/2 scr(4)-200]);h=gcf;set(h,'WindowStyle','docked');
 title([' Freq vs Std dev. of ST ' Afilemits ] ,'color', 'r', 'FontSize',16);
  end; set( gca,  'Units', 'normalized', 'FontUnits', 'points',    ...
 'FontWeight','normal',  'FontSize', fonts16, 'FontName','Times'  ) ;
if Afilemit == 232 , QQM= 0.001; 
 else  QQM= 0.001;   
end; minA1FSTDf=min(A1FSTD(f,:))-QQM;maxA1FSTDf=max(A1FSTD(f,:))+QQM;
TeE3 =(minA1FSTDf:(maxA1FSTDf-minA1FSTDf)/2:maxA1FSTDf);
 plot(A1FSTD( f ,: ) , 'LineWidth', 2 ,'color', col ); 
 titlefstd= ['  Freq vs Std dev. of ST matrix '] 
for i= 1: length( TeE3)  [ ST5{ i,1} , errsg ] = sprintf('%.3f',TeE3(i));end;  
set( gca, 'YTick',TeE3) ;   set( gca, 'YTickLabel', ST5 ) ;  
xlim([0  fM]);ylim([minA1FSTDf  maxA1FSTDf] ) ;  ...
date1 = [d(1:2),d(4:6),d(10:11),d(13:14),d(16:17)];
ylabel('Std Deviation','FontSize',16,'Color',cr);
xlabel('Frequency','FontSize',16,  'Color',cr );  
RA1 = [' Freq vs Std dev. of ST Record ' Afilemits  ] ; 
 title([RA1 ', beat ' int2str(f)  ', class ' Ca(K)] ,'color', cr, 'FontSize',16);
 FIGFfsd= [ 'zST' Afilemits  'FSD2890' cnptoqc B date1 int2str(f)  '.png' ];...
if ~Bsub,FIGFILEfsd= fullfile(PATFIG, FIGFfsd)  ; dbstop  2926; 
saveas( h2900fsd(s3i) , [ FIGFILEfsd    ] );
 end; if Bsub, FIGFIstf= ['zSTF2800' cnptoqc   B date1  int2str(f) '.png' ];
FIGFILEstf = fullfile(PATFIG, FIGFIstf ) ; h2265s(s3i) =figure(h2265( s3i ))
saveas(  h2265( s3i ), [ FIGFILEstf  ]  );%thesis 
 end; end;end; if  0&&AAflagn, h2900bl(s3i)=figure('Name', [ '2930 201'...
 cnptoqc ] ,'Position', [  20+K/10  30  screen(3)/2  screen(4)/2-10] );         
h=gcf; set(h ,'WindowStyle','docked'); 
plot( AXQRSYM{f  } , 'k' , 'LineWidth', 2 ) ; 
set(gca, 'XTick', 1: ( tM-1)/2  : tM );  set(gca,'XTickLabel',  [1:(tM-1)/2 : tM]) ; 
ddTBE2= 0.01 ; minAXQRSYM= min( AXQRSYM{f } )-ddTBE2 ;
maxaxqrsym= max( AXQRSYM{f } ) +ddTBE2 ;
celstr201=minAXQRSYM: (maxaxqrsym-minAXQRSYM)/2: maxaxqrsym;
ylim([minAXQRSYM  maxaxqrsym]) ;  xlim([0  201]) ; 
for i= 1: length( celstr201) [ ST1{ i,1} , ersg] = sprintf('%.2f', celstr201(i)) ;  end;
 set(gca, 'YTick', celstr201 ) ;   set(gca,'YTickLabel',ST1); 
set(gca,'Units','normalized', 'FontUnits', 'points', 'FontWeight', 'normal',...
 'FontSize', fonts18, 'FontName','Times'); title(PbE ,'color', cb , 'FontSize',16);
ylabel('Amplitude, mV',   'FontSize', fonts16  , 'Color', 'b'  );
 xlabel(' 201 samples centered at R',   'FontSize' , fonts16  , 'Color', 'b' );   
 xr = ['201' CLAS{ca(K) } Ca(K)   'Record ' Afilemits int2str(f)  '.png'] ;    
 FIGFI201=[  xr B d(1:2),d(4:6),d(10:11),d(13:14),d(16:17)  ];
 FIGU201 = fullfile(PATFIG, FIGFI201) ;if DBUG, dbstop 2950; dbstop 2948; 
saveas( h2900bl(s3i) , [ FIGU201 ] );  
end; end; if s3i&& AAflagn,w=screen(3)-80;hei=screen(4)-80; 
 hbeat(s3i)=figure( 'Name', [ '2950'   cnptoqc  Ca(K)   ] , ...
 'Position', [ (0-K)/10  (0-K)/10  w  hei ]);h =gcf; set(h ,'WindowStyle','docked'); 
if Afilemit==111, U = Pb2( f )-10; V = Tef1( f )+10;
    
elseif Afilemit==201, U = Pb2( f )-80; V = Tef1( f )+80;  
 if f== 73 ,   U = Pb2(f)-100; V=Tef1(f )+10 ; end; 
 elseif   Afilemit==202 , U = Pb2( f )-20; V = Tef1( f )+25;   
elseif Afilemit==113, U = Pb2( f)-0; V =Tef1( f )+2;    
 elseif Afilemit==207, U =Pb2(f)-80; V=Tef1(f)+80;U=Pb2(f)-100;V=Tef1(f)+100;     
elseif Afilemit==208& isequal(Ca(K),'S'),U=Pb2( f)-60;V=Tef1(f)+35;   
U =Pb2(f)-40; V=Tef1( f)+15;   
 elseif Afilemit==209 , U = Pb2( f )-35; V = Tef1( f )+35;   
elseif Afilemit==210 & f ==1862 , U = Pb2( f )-10; V = Tef1( f )-10;
 elseif Afilemit==210    , U = Pb2( f )-80; V = Tef1( f )+5;
elseif Afilemit==222  , U = Pb2( f )-10; V = Tef1( f )+5;
elseif   Afilemit==223 , U = Pb2( f )-20; V = Tef1( f )+25;
elseif   Afilemit==231 || Afilemit==233 , U = Pb2( f )-20; V = Tef1( f )+2;
elseif  Afilemit==234 , U = Pb2( f )-20; V = Tef1( f )+2;
else U = Pb2( f )-50; V =Tef1( f )+40;  
end; if 1,   col= ck;    
 if U<=0, U=1; end; if (V> NS), V = NS; end;  ws= int2str( (S(f )-Q(f))) ; 
plot(U : V, xbl1( U : V) , col , 'LineWidth', 2); xlim([U  V]);wqrsf=(S(f)-Q(f))/360;
x_a=[ 'QRS duration= ' num2str(wqrsf,'%.2f') ' Sec'];%[legh,oh,plh,ts]=legend( x_a); 
set(gca, 'XTick', U:fix((V-U)/2):V); set(gca, 'XTickLabel', (get(gca, 'XTick'))) ;
 set( gca,  'Units','normalized', 'FontUnits', 'points', 'FontWeight','normal',...
'FontSize', fonts18, 'FontName','Times') ; if DBUG,  dbstop 2976; 
end; if Afilemit==201,
minxbf1uv=min(min(xbl1(U:V))-0.1,-0.5); maxxbf1uv=max(max(xbl1(U:V))+0.1,0.75) ;  
 elseif Afilemit==222,minxbf1uv=min(xbl1(U:V))-0.1;maxxbf1uv=(max(xbl1(U:V))+0.1);
minxbf1uv=min(min(xbl1(U:V))-0.1,-0.5); maxxbf1uv=max(max(xbl1(U:V))+0.1,1);
elseif Afilemit==223, %ax = gca; ax.XColor= col; ax.YColor =col;
    minxbf1uv=min(min(xbl1(U:V))-0.1,-0.5); maxxbf1uv=max(max(xbl1(U:V))+0.1,1);
elseif Afilemit==207,minxbf1uv=min(xbl1(U:V))-0.1; maxxbf1uv=(max(xbl1(U:V))+0.1);
elseif Afilemit==200 &f ==1580,  
    minxbf1uv=min(min(xbl1(U:V))-0.1,-0.3); maxxbf1uv=max(max(xbl1(U:V))+0.1,0.3);   
elseif Afilemit==200,  
    minxbf1uv=min(min(xbl1(U:V))-0.1,-0.5); maxxbf1uv=max(max(xbl1(U:V))+0.1,0.5);   
elseif Afilemit==208,% if  ishold( hfusion), hold off; end ;
    minxbf1uv=min(min(xbl1(U:V))-0.1,-0.5) ; maxxbf1uv =max(max(xbl1(U:V))+0.1,1) ; 
    else    minxbf1uv= min(xbl1(U:V))-0.1; maxxbf1uv= (max(xbl1(U:V))+0.1);
    minxbf1uv=min(min(xbl1(U:V))-0.1,-0.75); maxxbf1uv=max(max(xbl1(U:V))+0.1,1);
end; ylim([minxbf1uv  maxxbf1uv]) ;  
axis([ U V  minxbf1uv  maxxbf1uv ]); 
xdiff= [minxbf1uv:(maxxbf1uv- minxbf1uv)/2 : maxxbf1uv]; 
 TPMnanN=[ minxbf1:(maxxbf1- minxbf1)/2: maxxbf1] ;
for i= 1: length( xdiff) [ ST1xdiff{ i,1} , errmsg] = sprintf('%.2f', xdiff(i) ) ; end; 
set(gca, 'YTick', xdiff );set( gca,  'YTickLabel',ST1xdiff ) ; 
ax=get(hbeat(s3i),'CurrentAxes'); set(gca, 'XColor', col) ; set( gca, 'YColor',col ) ;
if 0, ylabel('Amplitude, mV',   'FontSize', fonts16  , 'Color', col  );
xlabel('Time in samples,360 samples per second', 'Color', col,'FontSize',16); 
end; title( PbE ,'color',col ,'FontSize' ,16 ); Raf=Ra(f); hbeats(s3i)=figure(hbeat(s3i));
ws;%  if text( Ra(f ),xbl1(Ra(f)) , [x_a ], 'color',col, 'FontSize', 15) ;end; 
 FIGFILc=[ cnptoqc  d(1:2),d(4:6),d(10:11),d(13:14),d(16:17) B ,'.png'];
FIGFILcf=fullfile(PATFIG, FIGFILc );  if DBUG&BL, dbstop 3004;  
saveas(hbeat(s3i),  [ FIGFILcf  ]); 
end; if length(hbeat)>10 &~isempty(intersect(Ra(f),R1000)),close(hbeat(s3i));end; 
end; end; end; end;end; if 30, if 21, if ~isempty(ACAPC), 
if Afilemit == 222, ACAPC= setdiff([  ACAPC ], [ 653  ]')';
elseif (Afilemit==232) , ACAPC = [  ACAPC 653 5 ] ;
end; if  length( ACAPC) >=2, BT=2; else BT= length( ACAPC);end; 
for  ai = 1 : BT   f =ACAPC( ai ); K2= f; 
 if Afilemit==220 &&f==60, zerror2=-0.2;
elseif (Afilemit==231 &&f==132)||(Afilemit==210&&f==310), zerror2=-0.0;       
 elseif Afilemit==234 && f==60, zerror2=-0.2; else  zerror2=-0.0;
 end;   Rafs= int2str(Ra( f)) ; N=2;  m=1; 
 if 1, hapc(ai) =figure('Name', [' 3020 APCTesting' Rafs Afilemits Ca(f)] ...
,'Position', [20  40  screen(3)-200 600 ]); 
 h =gcf ; set(h,'WindowStyle','docked') ; 
set( gca, 'Units' , 'normalized', 'FontUnits','points',  ... 
 'FontWeight', 'bold' , 'FontSize', fonts16, 'FontName', 'Times');
fprintf(' 3005%d Ca( f-1)=%c Ca(f)=%c(f+1)=%cPb2=%dpe2f =%d\n',...
ACAPC( ai) ,Ca(f-1) , Ca( f) ,Ca(f+1), Pb2(ACAPC(ai)) ,Pe2f( ACAPC(ai))) ;
 hapcs=figure(hapc(ai) ) ;  f1= int2str(f); U=Pb2( f) -10 ;  V =Tef1( f ) +10 ;
 end; if 10, if  BANN,  apc1= subplot(N,1,1 ); end; 
U=Ra(f-1)-100;V=Ra(f +1)+100; if (U<1),U=1;end; if (V >= NS),V= NS-100;end;
plot(U : V , xbl1( U : V) , cg  , 'LineWidth', 2 );    xlim([U  V]) ; 
if BL, line( [ U, V] , [0, 0] , 'Marker', '.'  , 'Color', 'k' ); 
    elseif Afilemit==213 , line( [ U, V] , [0, 0] , 'Marker', '.'  , 'Color', 'k' ); 
  elseif 1,  line([ U, V], [ 0,0 ] , 'Marker' , '.', 'Color', 'k' , 'LineWidth', 1);        
end; set( gca, 'Units' , 'normalized', 'FontUnits','points','FontUnits', 'points', ... 
 'FontWeight', 'normal', 'FontSize', fonts16, 'FontName', 'Times');
 minxdfc  = min(xbl1(U:V) )-0.2 ; maxxdfc =max(xbl1(U:V) )+0.2 ;  
 fyxdfc= [   minxdfc   0  maxxdfc ] ; set( gca,  'YTick' , fyxdfc  ) ; 
 for i= 1: length( fyxdfc ) [ STfyxdfc{ i,1} , errm] = sprintf('%.2f', fyxdfc(i)); end; 
  set( gca,  'YTickLabel',  STfyxdfc ) ;  ylim([ minxdfc maxxdfc  ]) ;  
A3=[ U  floor((V+ U-1)/2)  V ];set( gca,'XTick', A3);set(gca,'XTickLabel', A3 ) ; 
A1B  = [  ' Class' Ca( f ) '   Beat '  int2str(f) ' Record '    Afilemits  ]  ;
title(['Channel '  lead1s ' of  class ' Ca(f) ', Beat '  int2str(f) ', Record ' Afilemits],...
  'color', 'r' ,'FontSize',16); zerr = zerrA1( f ) ;
  if  Afilemit ==100, zerr34f =0.2; zerr34fm1=0.2;  zerr34fp1 =0.2;  
elseif Afilemit==117 & f==615,zerr34f =zerrf1(f);zerr34fm1=.02;zerr34fp1=zerrA1(f+1); 
elseif Afilemit==200 & f==296,zerr34f=zerrf1(f); zerr34fm1=.02;zerr34fp1=zerrA1(f+1);
elseif(Afilemit ==232& f==737),zerr34f=zerrf1(f);zerr34fm1=.02;zerr34fp1=zerrA1(f+1);
    elseif  Afilemit ==223, zerr34f =0.12; zerr34fm1=0.12;  zerr34fp1 =0.12;   
  else   zerr34f =0.2; zerr34fm1 =0.2;  zerr34fp1 =0.2;   
  end; if DBUG&BL, dbstop 3043;  end;  
  if  (Afilemit ==201 & f== 73)   & (Afilemit ~=114  & 1)   , ,
 text(Ra(f-1),xbl1(Ra(f-1))+zerr34fm1, [ int2str(f-1) Ca(f-1)],'color','r' ,'FontSize',15); 
text(Ra(f), xbl1(Ra(f)) +0.1 , [int2str(f) Ca(f) int2str(Ra(f)) ],'color','r','FontSize',15);
text(Ra(f+1),xbl1(Ra(f+1))+zerr34fp1,[ int2str(f+1) Ca(f+1)] , 'color','r','FontSize',15);
elseif  (Afilemit ==202 & f== 356) || 1, 
text(Ra(f-1),xbl1(Ra(f-1))+zerr34fm1, [ int2str(f-1) Ca(f-1)],'color','r' ,'FontSize',15); 
text(Ra(f),xbl1(Ra(f))+ zerr34f, [ int2str(f) Ca(f) int2str(Ra( f))],'color','r','FontSize',15);
text(Ra(f+1),xbl1(Ra(f+1))+ zerr34f, [ int2str(f+1)  Ca(f+1)] , 'color','r','FontSize',15);     
elseif  (Afilemit ==202 & f== 356) || 1, 
  
  
  end; if 0,  if 0,  text(Tb1f( f-1) , xbl1( Tb1f( f-1) ), [  'Tb' ] ,'color', 'r' ,'FontSize',15); 
  text(Tef1(f-1), xbl1( Tef1(f-1)), [  'Te' ] ,'color', 'r','FontSize',15 );
end;if 0&(Pe2f(f)-Pb2(f)>25), fprintf('3040f=%dPb2=%dPe2f=%d\n',f,Pb2(f),Pe2f(f));
if  (Pb2(f) >0),  text( Pb2( f), xbl1( Pb2( f) ),[  'Pb' ] , 'color', cb ,'FontSize',15);
 end; if ( Pe2f( f)> 0), text(Pe2f( f) , xbl1( Pe2f( f) ),[  'Pe'] ,'color', cb,'FontSize',15 ); 
end; end;text(Tb1f( f) , xbl1( Tb1f( f) ), [  'Tb' ] ,'color', cb ,'FontSize',15); 
  text(Tef1(f-0), xbl1( Tef1(f-0)), [  'Te' ] ,'color', cb ,'FontSize',15 );
end; if 0&&(0) text(Pb2(f+1),xbl1(Pb2(f+1)),['Pb'],'color','r','FontSize',15); 
  text(Pe2f(f+1),xbl1(Pe2f(f+1)),[  'Pe'],'color','r' ,'FontSize',15); 
 end;  end; if  BANN,if 1,apc2= subplot(N,1,2); xlim([U  V]) ;   
plot( U : V , xbl2( U : V)  , 'm'  , 'LineWidth', 2 );   
if BL, line( [ U, V] , [0, 0] , 'Marker', '.'  , 'Color', 'k' ); 
elseif Afilemit==213 , line( [ U, V] , [0, 0] , 'Marker', '.'  , 'Color', 'k' ); 
  elseif 1,  line([ U, V], [ 0,0 ] , 'Marker' , '.', 'Color', 'k' , 'LineWidth', 1);    
end; set( gca, 'Units' , 'normalized', 'FontUnits','points','FontUnits', 'points', ... 
 'FontWeight', 'normal', 'FontSize', fonts16, 'FontName', 'Times'); 
if (Afilemit==101)&&(f==421),minxdf=min(xbl2(U:V))-0.01;maxxdf=max(xbl2(U:V))+0.01;
elseif (Afilemit==209&&f==83), minxdf= min(xbl2(U:V) ); maxxdf =max(xbl2(U:V) );  
elseif   (Afilemit==220)|| (Afilemit==200)|| (Afilemit==201)  || (Afilemit==207)        
    minxdf= min(xbl2(U:V) )-0.1; maxxdf =max(xbl2(U:V) )+0.1;  
 elseif (Afilemit==210), minxdf =min(xbl2(U:V))-0.02; maxxdf =max(xbl2(U:V))+0.01;  
 elseif (Afilemit==208),minxdf = min(xbl2(U:V))-0.12; maxxdf =max(xbl2(U:V))+0.1;  
 elseif (Afilemit==213), minxdf = min(xbl2(U:V))-0.12; maxxdf =max(xbl2(U:V))+0.1;  
else minxdf = min(xbl2(U:V) )-0.01  ; maxxdf =max(xbl2(U:V) )+0.01  ;      
end;  if  minxdf <=  0 && maxxdf  >= 0, fyxdf=  [   minxdf   0  maxxdf ] ; 
elseif  minxdf <= maxxdf, fyxdf= [ minxdf  maxxdf ] ;end; set( gca,  'YTick', fyxdf ) ; 
 for i= 1: length( fyxdf ) [ STfyxdf{ i,1} , errm] = sprintf('%.2f', fyxdf(i) ); end; 
  set( gca,  'YTickLabel',  STfyxdf )  ;  ylim([ minxdf  maxxdf   ]) ;  xlim([U  V]) ;  
A3=[ U  floor((V+ U-1)/2)  V ];set( gca,'XTick', A3);set(gca,'XTickLabel', A3 ) ; 
title(['Channel ' lead2s ' of class ' Ca(f)  ', Beat '  int2str(f) ', Record ' Afilemits],...
'color', 'r' ,'FontSize',16); fm1=  int2str(f-1); fp1=  int2str(f+1);
zerrapcf= zerrf2(f);  zerrapcfm1= zerrf2(f-1);  zerrapcfp1= zerrf2(f+1); 
rafm = int2str(Ra(f-1)); rafp =int2str(Ra(f+1)) ;  xlim([U  V]) ;
 da=[ d(1:2),d(4:6),d(10:11),d(13:14),d(16:17)];hapchapc=figure(hapc(ai));
end; end;FIGFILE=[ Ca(f) 'apc3095' Afilemits int2str(f) B da]; 
 FIGUFIL=fullfile(PATFIG,FIGFILE) ;if DBUG, dbstop 3091; 
 saveas( hapc(ai), [FIGUFIL], 'png'); 
end; end; end; if  ~isempty(CAFusion) ,
if length(CAFusion)>5  ,FTEST=1; else   FTEST=length(CAFusion);  end; 
 if Afilemit ==208, CTEST= [ 126  159 CAFusion'] ;
 else  CTEST= (CAFusion); end; 
elseif ~isempty(zlTw), CTEST=(zlTw);FTEST=1; else FTEST= 0 ; 
end;  for k = 1 : FTEST    if 10,
K2 =CTEST(k); f = K2; if K2==RL,  continue; end;if Afilemit==200 & f== 881,end; 
if f>1,U=Ra(f-1)-100; else U= Ra(f)-200; continue; end;%if K2==1,continue; end;
 hfusion(k)  = figure('NAME', [ Ca(f)  '3100 Fusion/unk/Ttest ' Afilemits] ,...
'Position', [32  -40 screen(3)-200 screen(4)/1-100]); 
h = gcf ;  set(h,'WindowStyle','docked') ; 
 fprintf(' 3090 f= %d Tb1f= %d Tef1 =%d  Pb2(f+1)=%d Pe2f(f+1)=%d\n',...
 f, Tb1f( f ) , Tef1( f ), Pb2(f+1), Pe2f(f+1) );A1TMA1= [ int2str(f+1) ]
 V=Ra(f +1)+100; if (U<=0),U=1; end;if (V>=NS) V=NS-1;end;
 subplot(2, 1,1);  plot(U : V, xbl1( U : V) , 'b' , 'LineWidth', 2 );
 if BL, line( [ U, V] , [0, 0], 'Marker', '.', 'Color', 'k' ); 
  elseif Afilemit==213 , line( [ U, V] , [0, 0] , 'Marker', '.'  , 'Color', 'k' );  
   elseif 1,  line([ U, V], [ 0,0 ] , 'Marker' , '.', 'Color', 'k' , 'LineWidth', 1);     
 end; set(gca,'Units', 'normalized' ,'FontUnits', 'points',  ...
 'FontWeight', 'normal',  'FontSize', 17 , 'FontName', 'Times' ) ; 
%xlabel('Time in samples, 360 samples  per second', 'Color', 'b','FontSize',16); 
if Afilemit==208,fuM=0.21;else fuM=0.1;end; 
minxdfc=min(xbl1(U : V))-fuM;  maxxdfc=max(xbl1( U : V))+fuM;    
 fyxdfc=  [   minxdfc   0  maxxdfc ] ; set( gca,  'YTick', fyxdfc  ) ; 
 for i=1: length( fyxdfc ) [ STfyxdfc{ i,1} , errm] = sprintf('%.2f', fyxdfc(i)); end;
  set( gca,  'YTickLabel',  STfyxdfc ) ;  ylim([ minxdfc maxxdfc ]) ;  
A1FMA2=[ U : floor((V- U-1)/2): V ]; xlim([U  V]) ;   fstr=  int2str( f )
A3=[ U  floor((V+ U-1)/2)  V ];set( gca,'XTick', A3);set(gca,'XTickLabel', A3) ; 
title( [' Channel '  lead1s  ' of class ' Ca(f) ', Beat  ' int2str(f)  ', Record ' Afilemits] ...
,'color', 'r' ,'FontSize', 15); fMnanN = [ int2str(f+2) Ca(f+2) int2str(Ra(f +2) ) ];
if Afilemit== 113,zerr43fm1= zerrA1(f-1); zerr43f= zerrw1(f);zerr43fp1= zerrf1(f+1) ; 
elseif Afilemit== 233,zerr43fm1=zerrA1(f-1); zerr43f=zerrw1(f);zerr43fp1=zerrf1(f+1);
else zerr43fm1= zerrA1(f-1); zerr43f= zerrw1(f);zerr43fp1= zerrf1(f+1) ; 
end;  if DBUG&BL , dbstop 3126; end; 
end; if 1, if (Afilemit== 208& f== 126)|| Afilemit== 208
            
text( Ra(f-1), xbl1(Ra(f-1))+0.39  , [ int2str( f-1 )   Ca(f-1) ] ,'color',  'r' ,'FontSize',15);
text(Ra(f)+2,xbl1(Ra(f))+0.31, [int2str(f ) Ca(f) int2str(Ra(f))]  ,'color','b' ,'FontSize',15); 
 text( Ra(f+1), xbl1(Ra( f +1))+0.41, [ int2str( f+1)  Ca(f+1) ] ,'color','r' ,'FontSize', 15); 
elseif Afilemit== 213|| Afilemit== 208|| Afilemit== 201|| Afilemit== 223
text( Ra(f-1), xbl1(   Ra(f-1))  , [ int2str( f-1 )  Ca(f-1) ] ,'color',  'r' ,'FontSize',15);
text( Ra(f) , xbl1(Ra( f )) , [ int2str( f ) Ca(f)  int2str(Ra(f ) ) ]  ,'color','b' ,'FontSize',15); 
 text( Ra(f+1), xbl1(Ra( f +1))  , [ int2str( f+1)  Ca(f+1) ] ,'color','r' ,'FontSize', 15 ); 
elseif Afilemit== 233,, 
text( Ra(f-1), xbl1(Ra(f-1))+ zerr43fm1 , [ Ca(f-1) ] ,'color',  'r' ,'FontSize', 15);
text( Ra(f ) , xbl1(Ra( f ))+ zerr43f   , [  int2str( f )  Ca(f)  ]  ,'color','b' ,'FontSize',15 ); 
text(Ra(f+1),xbl1(Ra(f+1))+zerr43fp1, [ int2str(f+1) Ca(f+1)] ,'color','r','FontSize',15);
elseif Afilemit== 213|| Afilemit== 201
 text( Ra(f-1),  maxxdfc-fuM , [ int2str( f-1 )  Ca(f-1) ] ,'color','r' ,'FontSize',15);
 text( Ra(f ) ,  maxxdfc-fuM  , [  int2str( f ) Ca(f)  ]  ,'color','b' ,'FontSize',15 ); 
 text( Ra(f+1),  maxxdfc-fuM , [ int2str( f+1)  Ca(f+1)] ,'color','r','FontSize',15 );  
elseif (Afilemit== 213|| Afilemit== 208|| Afilemit== 201) || 1 
text( Ra(f-1), xbl1( Ra(f-1))  , [ int2str( f-1 )  Ca(f-1) ] ,'color',  'r' ,'FontSize', 15);
text( Ra(f) , xbl1(Ra(f)), [  int2str( f ) Ca(f)  int2str(Ra(f) ) ]  ,'color','b' ,'FontSize',15);
 text( Ra(f+1), xbl1(Ra( f +1))  , [ int2str( f+1)  Ca(f+1) ] ,'color','r' ,'FontSize', 15 ); 
 end; if bon && (Pe2f(f) - Pb2(f ) )>15  if  (Pb2(f )> U), 
    text(Pb2(f), xbl1(Pb2( f) ),[  'Pb' ] , 'color', 'b' ,'FontSize', 15); 
 end; if (Pe2f(f) ),text(Pe2f(f), xbl1( Pe2f( f) ),[  'Pe' ] ,'color','b' ,'FontSize', 15);
 end; end; TBs2 = [ int2str(f-1) Ca(f-1) int2str(Ra( f-1 ) ) ]
 if bon&&(Tb1f(K2)>0),text( Tb1f(K2), xbl1(Tb1f( K2)),['Tb'],'color', 'b' ,'FontSize',15);
end;  if bon&&(Tef1O(K2) > 0),  
text(Tef1O( K2) , xbl1( Tef1O( K2) ),[  'Te'  ] ,'color' , cb,'FontSize', 15 );    
 end;  if bon  text( Pe2f(f+1), xbl1(Pe2f(f+1)) , 'Pe' ,'color', 'r' ,'FontSize', 15 ); 
    text( Pb2(f+1 ), xbl1(Pb2(f+1 )) ,'Pb' ,  'color', 'r' ,'FontSize', 15);
 end;if 0, text( Q(f ), xbl1(Q(f)) , 'Q'  );   text(S(f ), xbl1(S(f )) , 'S' ,'FontSize', 15);  
   if (Rp(f ) ) && (~isempty(Rp)) , text(Rp(f ), xbl1(Rp(f )), 'rp'  );    end;
  end;   NS1= Ra(f) ; Rg= Ra(f);  Te2rang(k ) =  Pb2(f+1)- 1 -Tb1f(f) -1 ;   
%  [slmaxc{f} ,slmaxic{f}  ]= max(slopes (  Limit ,Pb2(f+1)- 1 , xbl1) );
first_max=xbl1(Ra(f))- xbl1(Q(f)); maxi= maxi+((first_max -maxi)/filter_param);  
slope_thresh=(thresh_param/16)* maxi; pt_thresh=pb_param* slope_thresh;
    j=1;  % for i= Pb2(f+1)  : -1:   Tb1f(f )   + TeRangef(f) +1 
    for i = Q(f+1)-1  : -1 :  Tb1f(f ) -20    if (i <=0), i=3; end;
 if  ( abs(slope(i,xbl1)) > pt_thresh) ...
 && (abs(slope(i - 1,xbl1))> pt_thresh) %&& (slope(i+2)>pt_thresh)
 TE2v(f,j) =i;   j=j+1; break; 
end; end; if  exist('TE2v' , 'var'),  TE2f2( f )=TE2v(f,1);  end;
 if  j-1>0, TE2f1(f) = TE2v(f,j-1) ; Te2fzlTw( k)= TE2f1(f);
 else continue; end;  te =Tef1( f );  tb = Tb1f(f )  ;
if  ( (Tef1(f)-Tb1f(f)) <=0),
fprintf('3171 f%dTb=%d Te(K2)=%dTE(i)=%d Pb2(f+1)=%d Pe2f=%d\n',...
   f, Tb1f( f ) , Tef1( f ), TE2f1(K2), Pb2(f+1), Pe2f(f+1) );
end; end; if 10, subplot(2, 1,2); A1TMA1= [ int2str(f+1)  ]
 plot(U : V, xbl2(U : V) , cm, 'LineWidth', 2 ); 
if BL,  line( [ U,V] , [0, 0], 'Marker','.' , 'Color', 'k' ); 
 elseif Afilemit==213 , line( [ U, V] , [0, 0] , 'Marker', '.'  , 'Color', 'k' );
 elseif 1,  line([ U, V], [ 0,0 ] , 'Marker' , '.', 'Color', 'k' , 'LineWidth', 1);      
end; set(gca,'Units', 'normalized' ,'FontUnits', 'points',  ...
 'FontWeight', 'normal',  'FontSize', 16 , 'FontName', 'Times' ) ; xlim([U  V]) ; 
if Afilemit==210, fuM2=0.01; elseif  Afilemit==208, fuM2= 0.21; else fuM2=0.1; 
end;minxbl2 = min(xbl2(U:V) )-fuM2;  maxxbl2  =max(xbl2(U:V) )+fuM2 ;
fyxbl2=  [  minxbl2   0  maxxbl2 ] ; set( gca,  'YTick', fyxbl2 ) ; 
 for i= 1: length( fyxbl2 ) [ STfyxbl2{ i,1} , err] =sprintf('%.2f', fyxbl2(i)); end;  
 set( gca,  'YTickLabel',  STfyxbl2 )  ;  ylim([ minxbl2 maxxbl2  ]) ;  
A3=[ U  floor((V+ U-1)/2)  V];set( gca,'XTick', A3);set(gca,'XTickLabel', A3 ) ; 
title([' Channel ' lead2s  ' of  class ' Ca(f)  ', Beat  ' int2str(f) ', Record ' Afilemits] ...
 ,'color', 'r' ,'FontSize', 16);  if Afilemit== 124,
 zerrep2fm1=zerrA2(f-1);  zerrep2f =zerrA2(f);zerrep2fp1=zerrA2(f+1);
  elseif Afilemit==202 , zerrep2fm1=0; zerrep2f =-0.1; zerrep2fp1= -0.1;   
elseif Afilemit==223,
    zerrep2fm1=zerrA2(f-1); zerrep2f =zerrA2(f); zerrep2fp1=zerrA2(f+1); 
elseif Afilemit==233&f==394,
    zerrep2fm1=zerrf2(f-1); zerrep2f=zerrf2(f); zerrep2fp1=zerrf2(f+1);
else zerrep2fm1=zerrA2(f-1 ); zerrep2f = zerrA2(f ); zerrep2fp1=zerrA2(f+1); 
 end;if bon,   if DBUG, dbstop  3196;   end; 
if  Afilemit==223 &&f== 191,
     zerrep2fm1=zerrA2(f-1); zerrep2f =zerrA2(f); zerrep2fp1=zerrA2(f+1); 
 text(Ra(f-1),xbl2(Ra(f-1))+zerrep2fm1, [int2str(f-1) Ca(f-1) ] ,'color','r','FontSize',16); 
 text( Ra(f ) , xbl2(Ra(f))+zerrep2f , [ int2str(( f ))  Ca(f)  ],'color','b', 'FontSize' ,16 );  
 text(Ra(f+1),xbl2(Ra(f+1))+zerrep2fp1,[int2str(f+1) Ca(f+1)],'color','r','FontSize',16); 
 else
     text(Ra(f-1),xbl2(Ra(f-1))+0, [ int2str(f-1) Ca(f-1)] ,'color','r',  'FontSize',16);
 text( Ra(f ) , xbl2(Ra(f))+0 , [ int2str(( f ))  Ca(f)  ],'color','b' , 'FontSize' ,16 );  
 text(Ra(f+1), xbl2(Ra(f+1))+ 0, [ int2str(f+1) Ca(f+1)],'color','r', 'FontSize',16); 
 end; end; if bon if Pe2f(f)-Pb2(f)>20, 
     text(Pb2(f ),xbl2(Pb2(f)),['Pb'],'color','r' ,'FontSize',15);
    text(Pe2f(f  ),xbl2(Pe2f(f  )), [  'Pe'] ,'color','r' ,'FontSize',15); 
end;if Pe2f(f+1)-Pb2(f+1)>21,
    text(Pb2(f+1),xbl2(Pb2(f+1)),['Pb'],'color','r','FontSize',15);
  text(Pe2f(f +1),xbl2(Pe2f(f+1)) , [ 'Pe'], 'color','r' ,'FontSize',15 ) ;
end; end;end;hfusions = figure(hfusion(k) );  
FIGFILE=['F3190' Afilemits B d(1:2),d(4:6),d(10:11),d(13:14),d(16:17)];
FIGUFILE = fullfile(PATFIG, FIGFILE);if DBUG, 
 saveas(  hfusion(k)  , [FIGUFILE '_' int2str(f)], 'png')
end;end; if  ~isempty(ACPVC) , VB=1; 
  f=1; first_max = xbl1(Ra(f)) - xbl1(Q(f));   maxi= slomax250;  
  filter_param =16; thresh_param= 2;  pt_param = 0.2;
  maxi2= maxi  + ( ( first_max - maxi)/ filter_param ) ;
slope_thresh=(thresh_param/16)*maxi2; pt_thresh=pt_param*slope_thresh;
fprintf(' 3200 maxi=%f  filter_param%d pt_param%f  threshparam=%d\n',...
 maxi, filter_param ,pt_param ,thresh_param) ;   %pt_thresh= pt_thresh/100;
for  k =  1 : VB*1   if 1,  j=1; f=ACPVC( k) ;
  if f==1 continue; end;    % if C(f) == 'x',  continue; end;   
 Rg =Ra(f); strvpch= [ ' 3230  VPC Testing  ' num2str( Ra(f)) ' ' Afilemits ] ; 
 hVPC =figure('Name',  strvpch ,  'Position',...
[ 10 10  screen(3)/2 screen(4)-30]);  h = gcf ;  set(h,'WindowStyle','docked') ; 
    end; if   BANN,    subplot(2,1,1); grid off;
V=Ra(f+1)+100;U=Ra(f-1)-100 ;if (U<=0),U=1;end; if (V>=NS) V=NS-1;end;
plot(U : V, xbl1( U : V) , cyan  , 'LineWidth', 2 ); 
if BL, line( [ U, V] , [0, 0] , 'Marker', '.', 'Color', 'k' ); 
elseif Afilemit==213 , line( [ U, V] , [0, 0] , 'Marker', '.'  , 'Color', 'k' );
 elseif 1,  line([ U, V], [ 0,0 ] , 'Marker' , '.', 'Color', 'k' , 'LineWidth', 1);      
end; set(gca,  'Units', 'normalized',  'FontUnits',  'points', ... 
 'FontWeight', 'normal', 'FontSize', 15 , 'FontName', 'Times' ); 
  %'FontWeight',  'bold', 'FontSize', 15 , 'FontName', 'Times' ); 
if Afilemit==100,minxbl1=min(xbl1(U: V))-0.2; maxxbl1=max(xbl1(U: V))+0.2; 
else    minxbl1=  min(xbl1(U: V) )-0.1; maxxbl1=  max(xbl1(U: V))+0.1 ; 
end; fy1=[ minxbl1 0 maxxbl1] ; set( gca,  'YTick', fy1 ) ;
ylim([ minxbl1    maxxbl1 ]) ;   xlim([ U  V ]) ;   
 for i= 1: length( fy1 ) [ ST1{ i,1} , errsg] = sprintf('%.2f',  fy1(i) )  
end;  set( gca,  'YTickLabel',  ST1 ) ;  ylim([ minxbl1  maxxbl1 ]) ;
A3=[U  floor((V+ U-1)/2)  V ]; set( gca,'XTick', A3);  
set(gca,'XTickLabel', A3) ;  xlim([U  V]) ; SMnanN2=int2str(Ra(f+1))
title(['Channel 1 ' lead1s ' of Class ' Ca(f) ', of record ' Afilemits ' Beat ' int2str(f)],...
    'color',cr,'FontSize',16); if DBUG&BL, dbstop 3246; end;
if f>1, text(Ra(f-1),xbl1(Ra(f-1)),[int2str(f-1) Ca(f-1)],'color','b','FontSize',14); end; 
text(Ra(f), xbl1(Ra( f )), [ int2str(f) Ca(f) int2str(Ra(f)) ],'color','b','FontSize',14); 
text(Ra(f+1),xbl1(Ra(f +1))  , [int2str(f+1) Ca(f+1) ] ,'color','b', 'FontSize' ,14 );

wpb= Pe2f(f) -Pb2(f) ; if  ~Bthesis if  (Pb2(f-1) >U), 
 text( Pb2(f-1) , xbl1( Pb2(f-1)),[ 'Pb'] , 'color', 'b', 'FontSize' ,14 );
end; if  ((f-1)> 0)&& ( Pe2f(f-1)> U)
 text(Pe2f(f-1) , xbl1( Pe2f( f-1) ),[  'Pe' ] ,'color', 'b' , 'FontSize' ,14); 
end; if (( f-1)>0), text(Tb1f( f-1), xbl1(Tb1f( f-1)),[ 'Tb'] , 'color', 'b', 'FontSize' ,14); 
 end; if ((f-1)>0) &&(Tef1( f-1)>0) text(Tef1(f-1), xbl1(Tef1( f-1)), ['Te' ] ,'color','b');
end;  if  (wpb >30),  if  ( Pb2(f)>U),
     text( Pb2(f) , xbl1( Pb2(f)),[ 'Pb'] , 'color', 'b', 'FontSize' ,14 );
end; if (Pe2f(f) >U),text(Pe2f( f) , xbl1( Pe2f(f) ),[  'Pe'] ,'color', 'b' , 'FontSize',14); 
end; end; if 0, text(S( f) , xbl1( S( f) ),  [  'S'  ] ,'color', 'b', 'FontSize' ,14 ); 
end; if (f>0)&&(Tb1f(f)>0) text(Tb1f(f), xbl1(Tb1f(f)),['Tb'] ,'color','b','FontSize',14);
end; if (Tef1(f)>0), text(Tef1(f) , xbl1(Tef1(f) ),[ 'Te' ] ,'color', 'b', 'FontSize',14); 
 fprintf('3220 f=%dTb1f=%dTef1=%dPb2(f+1)%dPe(f+1)%d Ra(f+1)%d\n',...
  f, Tb1f( f) , Tef1( f ) , Pb2( f+1), Pe2f( f+1), Ra(f+1)  );          
 end;  end; TrangeTE(f)= Q(f+1) -  S(f ) ; 
for i = Q(f+1) : -1: S(f ) +2 % for i= Pb2(f+1) : -1: Tb1f(f ) +TeRangef(f) +1
 if (i >= (NS - 5)) i= NS -5;  end;
 if (abs(slope(i,xbl1)) >pt_thresh) && (abs(slope(i-1,xbl1))>pt_thresh) ...
 %&& (slope(i+2)> pt_thresh)
 TE(f,j) =i-1;   j=j+1;        
 end; end; if  exist( 'TE(f,1)' ,'var') ,  TEf1(f)=TE(f,1);   end;
 fprintf(' 3240 f=% d  %d S=%d   \n',f ,Ra(f),S(f) );
    if  j-1, TEf(f) = TE(f,j-1);   fin1 =find(f== zlTw) ;
 if  (TEf(f) <1000), %text( TEf(f) ,  xbl1(TEf(f)) ,'TEf' ,'color','g' );  
 end;end; if  exist( 'TE(f,1)' ,'var'), % text(TEf1(f),  xbl1( TEf1(f)),'TEf1' ,'color',  'g');
end; if  exist('TE(f,1)' ,'var'),  ltE = length(TEf);  ltb =length(Tb1f); tw= min(ltE, ltb) ;
 [zlTEfw ]= find( ( TEf(1:tw) - Tb1f(1: tw ) )<= 0)  ;        
end; end;  if BANN, subplot(212); 
plot(U : V, xbl2( U : V) , 'm' , 'LineWidth', 2 );
 if BL, line( [ U, V] , [0, 0] , 'Marker', '.'  , 'Color', 'k' ); 
 elseif Afilemit==213, line( [ U, V] , [0, 0] , 'Marker', '.'  , 'Color', 'k' ); 
 elseif 1,  line([ U, V], [ 0,0 ] , 'Marker' , '.', 'Color', 'k' , 'LineWidth', 1);     
 end;  set(gca,  'Units', 'normalized',  'FontUnits',  'points',       ... 
  'FontWeight', 'normal', 'FontSize', 15 , 'FontName', 'Times' ); 
if 10,AMP_S=[ (U:fix((V-U)/2):V) ] ;  set(gca,'XTick', AMP_S) ;
 set(gca,  'XTickLabel', (U:fix((V-U)/2):V)  ) ;     xlim([U  V]) ;   
 minxdfc  = min(xbl2(U:V) )-0.1;  maxxdfc =max(xbl2(U:V) )+0.1 ;  
 fyxdfc=  [   minxdfc   0  maxxdfc ] ; set( gca,  'YTick', fyxdfc  ) ; 
 for i= 1: length( fyxdfc) [ STfyxdfc{i,1} , errm] = sprintf('%.2f', fyxdfc(i)); end; 
  set( gca,  'YTickLabel',  STfyxdfc )  ;  ylim([ minxdfc maxxdfc  ]) ;  
end; wpbfp=  Pe2f( f+1)-Pb2( f +1) ;
xbl2Rafm =xbl2(Ra( f-1 ));xbl2Raf =xbl2(Ra( f ));  xbl2Rafp =xbl2(Ra( f+1 ));
xbl2Rafum =xbl2(Ra( f-1 ));xbl2Rafu =xbl2(Ra( f ));  xbl2Rafup =xbl2(Ra( f+1 ));
title([ 'Channel 2 ' lead2s ' of class ' Ca(f) ', of record ' Afilemits ' beat ' int2str(f)],...
'color','r','FontSize',16);
 if 0, if Afilemit==203
text(Ra(f), xbl2(Ra(f))+zerrA2(f), [int2str(f) Ca(f) int2str(Ra(f))],'color',cb,'FontSize',14);
 else text(Ra(f), xbl2(Ra( f )), [ int2str(f) Ca(f) int2str(Ra(f )) ],'color',cb,'FontSize',14);
 end; end; end; FIGFILE=[Ca(f ) 'VPC3700' Afilemits 'beat' int2str(f) B ...
 d(1:2),d(4:6),d(10:11),d(13:14),d(16:17)  '.jpg' ];
FIGUFILE=fullfile(PATFIG,FIGFILE); if DBUG,   dbstop 3301;  
saveas( hVPC, [ FIGUFILE ] );

 end; end;  end; if  (~isempty(zlTw) ), for i = 1:  length(zlTw) -2
 f= zlTw(i); Czltw(i)=Ca( zlTw(i)); czltw(i) =ca(zlTw(i) );Rte(i)= Ra(zlTw(i));
if ((Tef1(f)-Tb1f(f))<=0),fprintf('3300f%dCz=%c%cRa=%dS(f)%dTb1f%dTef1%d\n',...
  f , Czltw(i) , Ca(f+1) , Ra(f), S(f) ,Tb1f(zlTw(i)) ,Tef1( zlTw( i)) ) ;
end; end ; end ;  if 10, RRSUM =0;  %wavevaluate4(xbl1')  
COL220 = length(C);K1= 0 ; z =0; z2= 1; K= 0 ; zcontinue =0 ;zlen16 = [];   
 % if Afilemit ==220,RO(1)=[];COL220=length(C)-1;  C(1)= [] ; CO(1)=[]; end; 
   ROL= length(RO) ; ROL4= ROL-5;  RR2NDURe=[]; DUR=[]; zlen2 =[];
   if Afilemit == 200, ROL4= ROL-5; end; ROL4= ROL-5;  RLm3= RL-4;
   A2=0; RRPRDUS=0; dtwA= 0;  distwpsum=0;  clear tst fst ; 
end; for   i = 1 :  COL220 
   if  ((C(i)  == 'N')  || (C(i) == 'L') ||  (C(i)  == 'R') ...
 ||  ( C(i)  == 'j')   || (C(i)  == 'e')  ...
 ||  ( C(i)  == 'a') ||  (C(i)  == 'J')   ||  ( C(i)  == 'A') ||  (C(i)  == 'S')   ...
 ||  ( C(i)  == 'V') ||   (C(i)  == 'E') ||  ( C(i)  == 'F') ...%  || (C(i)  == 'x')
 ||  (C(i)  == 'Q')     ||  ( C(i)  == '/')   || (C(i)  == 'f')    || (C(i)  == 'n')  ) 
 K = K +1;  Rg  =  RO( i );     [ IN rov2 ] = find (Ra == Rg);
 if   Ra( K)-300 < 0 , zcontinue=   zcontinue +1; %    continue;
end;   K1=K1 +1;
 if  (K  <  RaL ),  RR1NDU( K1  ) =  ( Ra( K+1) - Ra(K ) );
else    RR1NDU(K1 ) = (Ra(  2) - Ra(1) )  ;
end;     RRSUM = RRSUM + RR1NDU( K1);
   if  ( K >1),  if Afilemit ==  220,  RRPRDU( K1) = ( RO( i) - RO(i-1));
     else   RRPRDU( K1) =  ( RO( i ) - RO( i-1) );
     end; else   RRPRDU( K1) =  ( RO( i+1 ) - RO( i ) );
     end;   RRPRDUS =     RRPRDUS +  RRPRDU( K1);
zlen2 =   [  zlen2 ; [Tef1( K )  - Pb2( K)   ] ] ; %jdtwdistfig16  
 if K==1, distwp( K) = jdtwdist( AXQRSTF( i ) , AXQRSTF(i+1 ) );  
 elseif 1,  distwp( K) = 0;  end;  distwpsum =  distwp( K) + distwpsum ;   
 end;  end ; if 1,  RRPRDUSAvg=  RRPRDUS / K1; 
 AFT = 0 ; A1C=[]; [   zlensort2  ] =  sort(  zlen2' ) ; clear MA* ; 
%  if Afilemit == 201 , C = CA; end;  
dirsig= [ ' BandPass Filter:'  num2str(BP1) ' : ' int2str(BP2)  ] ; 
 %  [ RR1NDURmax1  RR1NDURI1 ]=  max(RR1NDU  )
NB = length(Ca );  z4 = 4;j =0;    RRA =   RRSUM/  K1 ;zlTb1f
RRA2 =   RRSUM/  NB;     format  short g;clear  GBL  pbls1 ;
A1791=[]; AX=[];beats=[];class=[]; zlen=[];   in = intersect(RO, Ra) ;
diff  =setdiff(RO , Ra) ;  el =0; zrli=0;    K=0;  K3=0 ;
LC =length( C ) ;  ERRorI=[]; ROL = length(RO );  
if  LC ~= ROL , error('2804');  end; ROLm3= ROL-4;   
 if 0,   if   Afilemit == 105  , REE= [ 5561  12625   ]  ; 
elseif Afilemit == 108 ,  REE= [  644733    ] ;
 elseif Afilemit == 200 ,  REE= [  549292    ] ;   
 elseif Afilemit == 208 ,  REE= [ 498665 ] ;
R208 =  [  498665  499495    499292  499084 498864   611045 ];
      elseif   Afilemit == 213 , REE= [ 117520   120663  ]  ;
elseif   Afilemit == 222 , REE= [ 192786   201552   194304  ]  ;
elseif Afilemit ==223, REE=[  81636 ];elseif  Afilemit ==232 , REE=[ 344483];
    elseif   Afilemit == 233 , REE= [ 500   36740  ]  ;
end;  end; if  Afilemit == 118,  x= xbl1 ;  xts= xbl1;
    elseif   Afilemit == 222 ,    x= xbl1 ;  xts = xbl1;
     elseif   Afilemit == 105 ,    x= xbl1 ;  xts = xbl1;   
    else    x= xbl1 ;     xts= xbl1;  clear       xbl3;
    end;  mf= 1; Mf= 100;   fM = 100; tM = 201;  
  clear ANNOT ;  % if  isequal( B,'BLN') xts= xbl1;  x = xbl1;   end;
clear F1 xd2 x   xlf1  xr  xlf2     fy2  xr  ca1  X*   xbl3  ; 
 end;  end; if 37, for i = 1: length( C)  if  (( C(i) == 'N')  ...
  || (  C(i)  == 'L') ||  (C(i)  == 'R') ||  (C(i)  == 'j')   || (C(i)  == 'e')  ...
  ||  ( C(i)  == 'a') ||  (C(i)  == 'J') ||  ( C(i)  == 'A') ||  (C(i)  == 'S')  ...
  ||  (C(i)  == 'V') ||   (C(i)  == 'E') ||  ( C(i)  == 'F') ||  (C(i) == 'Q')  ...
  ||  ( C(i)  == '/')   || (C(i)  == 'f')    || ( C(i)  == 'n')  )  %  || (F(i)  == 'x')
% if  ( start ~=1)   R1=Range(i);  end; % c6=[ Afilemits int2str( Ra(i-1)) ];  
 if 1, Rg  = RO( i ) ;  [ K ] = find ( Ra  == RO( i) ) ; 
 if Afilemit == 108 ,   if    find (Rg  == REE)   continue;  end;  end;
  if Afilemit == 200 ,   if   find (Rg  == REE) %  continue; 
end;  end;  if Afilemit == 208 ,   if    find (Rg  == REE)  end;  end;
 if Afilemit == 222 ,   if    find (Rg  == REE)  end;  end;
if 0, if  K >=RaL-2 ,  fprintf(' 3350 i %d %d %d \n', i , K , K3 ); % break ;
end;  if  Rg - 315< 0 ,   zcontinue=   zcontinue +1;     continue;           
 elseif Afilemit == 202 && Rg - 351 < 0 ,zcontinue= zcontinue +1; continue;
elseif  Afilemit == 232 && Rg-495< 0, zcontinue= zcontinue +1;  continue;
elseif  Afilemit == 106 && Rg-365 < 0, zcontinue= zcontinue +1;continue;
 end;  if Afilemit == 105 , if  find(Rg== REE),  continue; end;  end; 
if Afilemit == 213 , if    find (Rg== REE)  end;  end; 
if Afilemit == 223 , if    find (Rg== REE)   continue;   end;  end; 
if Afilemit == 233,   if  find (Rg== REE)  continue;     end;  end;
if   find (Rg== REE),  continue;   end;
if Q(K)<=0 || S(K)<=0,fprintf(' 3360 K3%d%d%d\n', K3, K, i ); continue; end;
 end; if  (length(K) >= 2 ),    fprintf(' 3365 i%d%d%d \n', i,K(1), K(2)); K=K(1); 
 end;  if  ~rem( i , 500 )  fprintf('3389 i %d %d %d \n' , K3 , K , i   );end;
if Pe2f(K)<= 0, Pe2f(K) = Q(K); end;if Pb2(K)<=0, Pb2(K)= Pe2f(K); end;
if  Tb1f(K)<=0,Tb1f(K)=S(K); end;
if Tef1(K)<=0, AFT=AFT+1;Tef1(K)=Tb1f(K); end;
if  i-1>0,  if   ( c( i-1) == 28 ||  c( i-1) == 37 )  A1C =[A1C RO(i)  ] ; 
end; end;  K3 = K3 +1;  if Afilemit == 200,  R20= find( Ra(K)==549292);  end; 
 if  xts( Ra(K) )<=  0,  INVR(K) = 1 ;
 if  (S( K)  <= Tef1( K) )   [ STe( K )  STeI( K)]  =   max( xts( S(K): Tef1( K))) ;
elseif (Ra(K)<= Tef1( K) )  [ STe(K )  STeI( K)] = max( xts( Ra( K) : Tef1(K))) ;
elseif  (Tef1(K )<= Ra( K))  [ STe( K)  STeI( K)] = max( xts( Tef1( K): Ra( K))) ;
end; if (Q(K)<=S(K)) [xRac(K)  RacI( K)]= min(xts(Q(K): S(K))) ;
          Rac( K)= RacI( K) + Q(K ) ;
 elseif S(K) < Q(K),  [ xRac(K )  RacI( K)] = min( xts( S( K ) : Q( K)));
          Rac( K)= RacI(K) + Q( K ) ;
        end;
  else  INVR(K ) = 0 ; 
 if 0, if Afilemit~=213, [  xRac(K )  RacI( K)]  = max( xts( Q(K) : S(K)) ) ;  
          Rac( K)= RacI( K) + Q( K ) ;    
elseif Afilemit == 213, [  xRac(K)  RacI( K)] =  max( xts( Pb2(K):Tef1(K))) ;  
          Rac( K)= RacI( K) + Pb2( K ) ;    
end;end; if (S(K)<=Tef1(K)),[STe(K)  STeI(K)] = min( xts(S(K) : Tef1(K))) ;
elseif (Ra(K)<= Tef1(K)) , [STe(K)  STeI(K)] = min( xts(Ra(K) : Tef1( K))) ;
elseif  ( Q(K)<=Tef1( K)) ,  [ STe(K)  STeI( K)] =min( xts(Q(K) : Tef1(K) )) ;
 elseif ( Q( K)<= S( K)),[ STe(K )  STeI(K)]=min( xts( Q( K) : S( K) )) ;
end ; end ; 
FFINVR(K3) =  INVR(K) ;   % 12
  FFSTe(K3)  =STe(K ) ;     FF(K).Afilemit  =  Afilemit ;
  RaFF(K3)  =RO(i) ;  FFS(K3)=  S(K ) ;  FFC(K3)  = ca( K) ;  A1(K3)=C(i);  
  if (K+1)<= length(Ra), RN=RO(i +1); Rn=Ra(K+1);else  RN = NS; Rn=NS;
  end;  RRR(K3) =  ( Rn - Ra(K) ) /RRA ;  STb(K3) = Tb1f(K)-S(K);
if (RN~=Rn), fprintf('3385K=%d%c %c%c\n', K, CA(K-1), CA(K), CA(K+1));
      fprintf(' 3385   i=%d %d %c %d \n', i , c( i-1 ) , C( i +1) , c( i+1 ) );
end;if 0, fprintf('3395 K%d%dc%cPb%d%dQ%dRa%dS%dTb%dTe%d\n', ...
  K , K3 , F(i).c , Pb2(K ), Pe2f(K ) , Q(K), Ra(K) , S(K),Tb1f(K)  ,Tef1(K) ); 
fprintf(' 3395 i =%dRO(i)=%dRa=%dRa(K)  %d Ra(K-1)%dRO(i-1)%d\n',  ...
   i , RO(i),    Ra(K )  ,   Ra( K)  ,Ra( K-1) ,RO(i-1)  );
fprintf('3406 i=%d %d%d%d%d%d\n', i, c(i-2), c(i-1), c(i+1), c(i +2), c( i+3));    
 end;  if ( K-2 > 1 )  ...
 &&  ( ca( K-1 )  ==1   ||    ca( K-1 )   ==2    ||    ca( K-1 )   ==3  ) , 
 AVG(K3)=    ( Ra( K - 1) - Ra(K-2) )  ; 
elseif  K+2<=length(Ra) && ( ca( K+1)==1|| ca( K+1) ==2 || ca( K+1) ==3) , 
AVG(K3)=    ( Ra(K+2) - Ra(K+1) )  ;     
elseif   ( K-2 > 1 )  && ( c( i-2 )   ==1  ||  c( i-2 )   ==2 ||  c( i-2 ) ==3) ,  
AVG(K3)=    (RO(i-2) - RO(i-3))  ; 
 elseif   K+3 <= length( Ra) &&  (c( i+2)==1  || c( i+2) ==2  ||  c( i+2) ==3) , 
AVG(K3)=    ( RO(i+3) - RO(i+2) )  ;    
elseif ( i-4 >= 1 ) &&  ( c( i -3 )   ==1  || c( i-3 ) ==2 || c( i-3 ) ==3 ) ,    
AVG(K3)=    ( RO(i-3) - RO(i-4))  ;     
elseif  ( K+3<= length(Ra))&&  (c( i+3 )==1 || c( i+3) ==2 || c( i+3 )==3 ) ,            
AVG(K3)=    ( RO(i+4) - RO(i+3) )  ;   
 else      AVG(K3)=  ( Ra(1001) - Ra(1000) ) ;%( Rac(K) - Ra(K-1));   
end; if 1, if (K-1 > 0) FFRRPREV(K3) = (Ra( K) - Ra( K-1 ) ) ;  
    NFFRRPREV( K3) =  (Ra( K  )  - Ra( K -1 ) )  /   AVG(K3) ;     
else      NFFRRPREV( K3) =  (Ra( K + 1  )  - Ra( K ) ) / AVG(K3) ; 
    FFRRPREV( K3) =    (Ra( K  + 1 )  - Ra( K   ) ) ; 
end;  if K -2 > 0     FFRRPREV2( K3 ) = ( Ra( K - 1) - Ra( K -2) ); 
else  FFRRPREV2( K3 ) = ( Ra( K + 1) - Ra( K ) ); 
    end;   if K -1 > 0   BLPRE(K3) =    ( Tef1( K-1 ) -  Pb2(K-1) )  ; 
else  BLPRE(K3) =    ( Tef1( K+1 ) -  Pb2(K+1 ) )  ;  
end;  FFBLPRE(K3 ) =   1000 * BLPRE(K3 )/360 ;
 end; if K+1 <= length(Ra),FFRRPOST(K3) = (Ra(K+1) - Ra( K ) );  
else FFRRPOST(K3  ) =  (NS - Ra( K ) );  
 end;   end; if  K3==1 && 0,   n=8; 
 fprintf('3438%dK%dK3=%dc%cPb=%d%dQ%dR%dS%dTb1f%d%d\n', ...
i, K ,K3 , F(i).c , Pb2(K ), Pe2f(K) , Q(K ), Ra(K) , S(K),Tb1f(K),Tef1(K));
  h2Stockwell =figure('Name',['  3450 Stockwell Transform ' ], ...
      'Position',[screen(3)/2 30  screen(3)/2 screen(4)/2-100]);
 subplot(1,4,1);  plot( mf: fM ,  A1ST' );
 subplot(1,4,2);  plot( mf: tM,  A1ST);
 subplot(1,4,3);  plot(1: tM, A1TMA1(K3,:)); 
 subplot(1,4,4);  plot(1:fM,A1FMA2(K3,:)) ;  
   end; if 1, FFQTP(K3)= 1000*( Tp(K )- Q( K ) )/360; 
 FFRH(K3) = xts( Ra(K ) ) ;      FFQRSD(K3) = ( S( K)- Q( K ) );  
FFQRH(K3) = xts( Ra(K ))- xts( Q(K )) ; FFRSH(K3)= xts(Ra(K))- xts(S(K));
RPSTP(K3)=0.5 * 1000*(xts(Tp(K))-xts(S(K)))*(Tp(K)-Rp(K))/360;%10/7to265
 FFRSSLOPE(K3) =1000*(xts(Ra(K)) -xts(S( K)))*( Ra(K) - S(K)) /360 ;%11/8
 FFRSXA(K3)=0.5 *1000*(xts(Ra(K)) -xts( S(K) ))*(S( K)-Ra(K))/360;%0265
  PW(K3) = ( Pe2f(K)-Pb2(K)) ;   PWO(K3) = ( Pe2fO(K)-Pb2O(K)) ; 
% FFPW(K3) =1000*( Pe2f(K)-Pb2(K) )/360;
%FFTW(K3) =1000*( Tef1(K) -Tb1f(K))/360;
 TW(K3) =( Tef1(K) -Tb1f(K)); TWO(K3) =( Tef1O(K) -Tb1fO(K));
      if Afilemit == 220  ,   FFPW(K3) =( PW(K3 ) );   FFTW(K3) =(TW(K3)) ;
      else   FFPW(K3) =( PW(K ) );   FFTW(K3) =(TW(K)) ;
      end;
  if  ((Tef1(K) ==0) ||  (Tb1f(K)  == 0)) ,  FFTW(K3) =  0;    end; 
 if K+1 <= length(Ra) ,   TePb(K3) = Pb2(K+1) -  Tef1( K )  ;
 else   TePb(K3) = Pb2(K) -  Tef1( K-1)  ;  
end; if (( i < 10  || i >  NB-5 )  ||  (  FFTW(K3) <= 0) || (FFPW(K3) <= 0)) 
 fprintf(' 3450 K3=%d  K=%d i= %d  Ca= %cRg=%d\n',K3, K, i , Ca(K), Rg);
 fprintf([' 3460 c=%c Pb2=%d Pe=%d Q=%dRa=%dS=%dTb=%dTe=%d \n'], ...
  C(i)  , Pb2(K)  , Pe2f(K) , Q(K ), Ra(K) , S(K)  ,Tb1f(K) ,Tef1(K)  ); 
 fprintf(' 3453 i = %d RO(i)=%d S=%d Tb1f=%d Tef1(i) %d \n' ...
, i , RO(i), S(K)  ,Tb1f( K)  ,Tef1( K)   );
end; if ( K+2<=length(Ra) ),  FFRR2NDUR( K3) =( Ra( K+2)- Ra(K +1));
    else   z = z+1; FFRR2NDUR(K3 ) =  (Ra(z+2) - Ra(z+1) );  
 RR2NDURe = [ RR2NDURe  ; i  K1 Rg   (z+2)  (z+1)] ;
   end; if  (K+3 <=length(Ra)), FFRR3NDUR(K3) =( Ra( K+3)- Ra( K+2));    
  else  FFRR3NDUR(K3 ) =   (Ra(z+3) - Ra(z+2) );    
  end; if K+4 <= length(Ra) ,  FFRR4NDUR(K3) =( Ra(K +4)- Ra( K +3));  
  elseif K3~= z2+1,    FFRR4NDUR(K3) = FFRR4NDUR(z2+1) ;       
  else    FFRR4NDUR(K3) = FFRRPREV(K3) ;       
 end;    FFRR2AVG(K3)= (   FFRRPOST( K3) +  FFRRPREV(K3) )/ 2 ;        
  FFRR3AVG(K3)=(FFRRPOST(K3)+FFRRPREV(K3)+FFRR2NDUR(K3))/3;  
 if  K+5 <= length(Ra),     FFRR5NDUR(K3) = ( Ra(K+ 5)- Ra(K + 4) );
 elseif  K3-5 >= 1,      FFRR5NDUR(K3) =  FFRR5NDUR(K3-5);
 else   FFRR5NDUR(K3) =  FFRRPREV( K3);  
end; if  K+6 <= length(Ra),   FFRR6NDUR(K3) = ( Ra(K+ 6) - Ra(K + 5)); 
 elseif  K3~= z2,   FFRR6NDUR(K3) =  FFRR6NDUR( z2 ); 
else FFRR6NDUR(K3) =  FFRRPREV( K3);  
end; if  K+7<=length(Ra), FFRR7NDUR(K3) =( Ra(K +7) - Ra(K+ 6 ) );   
 elseif  K3~= z2,       FFRR7NDUR(K3) = FFRR7NDUR( z2 ); 
else       FFRR7NDUR(K3) = FFRRPREV( K3);    
end; if K+8<= length(Ra),  FFRR8NDUR( K3) = (Ra(K+8)- Ra(K+7));
 elseif  K3~= z2,      FFRR8NDUR( K3 ) =   FFRR8NDUR( z2 );
else FFRR8NDUR( K3 ) =    FFRRPREV( K3);  
end;  FFRR7AVG(K3) =(FFRRPREV2(K3)+FFRRPOST(K3)...
 +FFRR2NDUR(K3) + FFRR3NDUR(K3)   ...
  + FFRR4NDUR(K3) +   FFRR5NDUR(K3) + FFRRPREV( K3 ))/ 7;      
 FFRR4AVG(K3)  = ( FFRRPOST( K3) +  FFRR2NDUR( K3) ...
 +FFRRPREV(K3) +FFRRPREV2( K3))/4;
FFGRL(K3) =xts( Ra(K))-GP2(K) ; FFRRPR(K3)= 0 ; 
 Pb2Te(K3) =Tef1(K)-Pb2(K) ; Pb2R(K3) = ( Ra( K) -Pb2(K));
 RTe(K3) =Tef1(K)-Ra(K) ; 
if (K< 5|| K >NB-5),    fprintf(' 3485 i=%dK=%dc=%c Q =%dRa=%dS=%d\n',...
   i, K, C(i) , Q( K ),  Ra( K ) , S( K ));                 
 end; if (K< 5 || K > NB-5), fprintf('3489 i=%d K= %d Q =%d %d S=%d \n',...
  i, K  , Q(K),Ra(K) ,S(K) );     
end; if  BVER,
zrow = [ RaFF(K3)  FF(K).Afilemit FFC(K3)  FFPW(K3)  FFTW(K3)  ... %1-5
 Pb2R(K3) FFBLPRE(K3) TePb(K3) FFGRL(K3) FFQRH(K3)    ...  % 6-7,810
FFRSH(K3)  FFINVR(K3)   FFRSSLOPE(K3)   FFRSXA(K3)    ...   % 11-14
 RPSTP(K3)   FFQTP(K3)   FFQRSD(K3)   RRR(K3)  ...  % 15,17,18 %7
FFRRPREV(K3) FFRRPOST(K3) FFRR2NDUR(K3) FFRR2AVG(K3)  ... %19-22
FFRR3AVG(K3)   FFRR4AVG(K3)  FFRR7AVG(K3)  RRdur(K3) ...  %23-26,
 0  0 FFRR2NDUR(K3)  0  0 0 0  0       ...      %27-34 :8 
STb(K3)  FFFQRSD3(K3)  FFRH(K3) Pb2Te(K3)  NFFRRPREV(K3)    ...
FFSTe(K3)  ... %35,3740
 A1TMA1std(K3 )  A1TMA1mean(K3 )    A1TMA1min(K3 )  ...% 41,44
 A1FMA2std(K3 ) A1FMA2mean(K3 )    A1FMA2min(K3 ) ... % 44-46
  A1FSTDstd(K3)   A1FSTDmean(K3)   A1FSTDmin(K3 )  ...% 47-49
  %  RRPRDU( K3 )   FF(K).Afilemit   ... % 51
] ; elseif  ~BVER  
zrow = [ RaFF(K3) FFS(K3)  FFC(K3)  FFPW(K3)  FFTW(K3)  ... %12345
 Pb2R(K3) FFBLPRE(K3) TePb(K3) FFGRL(K3) FFQRH(K3) ...  % 6-7810
FFRSH(K3)  FFINVR(K3) FFRSSLOPE(K3)   FFRSXA(K3)   ...   % 11-14
 RPSTP(K3)   FFQTP(K3)   FFQRSD(K3) ...
 RRR(K3) FFRRPREV(K3) ...  % 15,17,18% [18,19,  26,27,39 ,47] 
  FFRRPOST(K3 ) FFRR2NDUR(K3)   FFRR2AVG(K3)  ...  % 19-22
FFRR3AVG(K3) FFRR4AVG(K3)  FFRR7AVG(K3) RRdur(K3)  ...  %2326,
 0    0  STb(K3) 0  0 0 0    0      ...      %27 -34 :8 
FF(K).Afilemit  FFFQRSD3(K3)  FFRH(K3) Pb2Te(K3) NFFRRPREV(K3) ...
FFSTe(K3)  A1TMA1std(K3 )  ... %3540
 A1TMA1mean(K3 )  A1FMA2std(K3 ) A1FMA2mean(K3)   ... %41,44
  A1FSTDstd(K3)   A1FSTDmean(K3)   RRPRDUSAvg  ...% 45,47
  A1TMA1min(K3 )     A1FMA2min(K3 )    A1FSTDmin(K3 )    ... % 48,50
  ] ; end;  AXQRS{K3}  = [ xts( Pb2( K)  : Tef1( K) ) ]' ;  
zlen(K3) =  [Tef1(K) - Pb2( K) ] ;
if  zlen(K3)<=0, fprintf(' 3524 i%d %d %d\n', i,K, K3);  end;  
if 0,fprintf(['3527 c=%cPb2=%d Pe=%dQ=%dRa=%dS=%dTb=%dTe=%d\n'], ...
 C(i)  , Pb2(K)  , Pe2f(K) , Q(K ), Ra(K) , S(K)  ,Tb1f(K)  ,Tef1(K)  ); 
 end; AX(K3, : )=[zrow];A1791=[A1791;  RaFF(K3)  FFS(K3)  FFC(K3)];  
DUR= [DUR;RRR(K3) NFFRRPREV(K3) FFRRPREV(K3) ...
RRdur(K3) RRPRDU(K3) ];% zrow = fix(zrow) ;
 DURAX(K3, : ) = AX( K3  , [ 18 ,  39  , 19,  26,47 ]); clear  Rg Rc RN ;  
end;   else   el = el +1;     zp(el)= F(i) ; zpp(el)= i  ;
 end; end; if 1,  NanGRL = find( isnan(FFGRL) ) 
 FEATM= { 'R' , 'S' , 'C' , 'PW' , 'TW' , 'Pb2R' , 'BLPRE' , 'TePb', ...    %8
'GRL' ,'QRH','RSH','INVeR','rsslop','RSXA','RPSTP', 'QTP','QRS', 'RRR',  ...%18
 'RPRE', 'RPOST', 'R2NDU', 'R2AVG', 'R3AVG', '4AVG', 'R7AV', 'RRAT', ...%26
 'RPRAV','RPOAV','R2NAV', 'R2avav','R3AVA','5AVG', 'R7AVA','RAVA', ... %34
 'FILE' ,   'FOURT' ,  'RH'  ,'STb'  ,  'NFFRRPREV' ,  'FFSTe' ... %35-40
   ,'A1TMA1std' ,  'A1TMA1mean' ,  'A1FMA2std' ,  'FMA2mean' ,  ...  %4144
'FSTDstd','fstdmean' , 'prduavg' ,'TMA1min','FMA2min',  'Fstdmin'  ... %50 
  ,'FILE' };% if track ,  dbstop 3590 ; end; if track ,  dbstop 3500 ; end;
zrowsize = size(zrow);  [ BLPREv  BLPREi ]  =   max(BLPRE) 
[ zlensort ]=sort( zlen' ,'descend'); [ BLmax   BLmaxI ]=max(FFBLPRE)
[ RR1NDURmax  RR1NDURmaxI ] = max( FFRRPREV  )  
  zrr2avg = find( FFRR2AVG <=0 );    c2= 'm';  c3= 'r';    
 fprintf('3520  Feat Extraction Complete \n'); zTws = find( FFTW<= 0) ; 
 [zPwS1  zPwS1I]= find( FFPW <= 0);  zPwS1row = FFPW(zPwS1I) ;  
 scof1=num2str(cof1) ; scof2=num2str(cof2) ;  uH= unique( A1)
 [ FFBLPOSTv  FFBLPOSTi ] = find(FFINVR<=0) ; [ AAr1 c1]=size(AX);
 screen =get(0,'ScreenSize')  ;  Ac200 = [11  13  21 191   448 962] ; 
clear A1ST  ANNT   PbE     ANNOT   AMP*  SUBTYPEFIELD  POS* ; 
clear xbl1 xbl2 in RW QW;  clear xRac  Rp  S1 SM  ps2 TPMnanN; 
  
[ s3x s4x]= size( AXQRS ) ;  if s3x==1, AXQRS= AXQRS' ; end;  
 A19 =zrowsize(2) ; AVr1= num2str(AAr1);AV= num2str(A19);   uC
 if (Afilemit == 105),  Ob=111 ; 
 elseif (Afilemit == 108),  Ob=111 ;  elseif (Afilemit == 109), Ob=111 ;
elseif (Afilemit == 111 )   Ob=111;    elseif (Afilemit == 114), Ob=111; 
  elseif (Afilemit == 116 ) ,  Ob=132; elseif  (Afilemit ==121),  Ob=120 ;   
elseif  (Afilemit == 124 ),  Ob= 100;% elseif (Afilemit == 207), Ob=121 ; 
elseif  (Afilemit == 200), Ob=121 ;    elseif  (Afilemit == 202), Ob=111 ;       
   elseif  (Afilemit == 208),Ob= 100 ;  elseif  (Afilemit == 210),Ob=111;  
elseif  (Afilemit ==215), Ob= 111;elseif  (Afilemit == 230),Ob= 111;
else  Ob=121;end;     Ob=111;
ROCNPT= find(ca== 5 | ca ==10);  Obs = int2str(Ob) 
 zlensort1820I= find(ca==1| ca== 2 | ca==3 | ca ==11| ca ==34);  
 RPSTP=find(ca ==4 | ca == 7 | ca == 8 | ca== 9); 
 A1nsv_indicesTF= find(ca==6);
 L1 =length( zlensort1820I) ; L2= length( RPSTP) ; 
L3 = length(ROCNPT) ;  L4= length( A1nsv_indicesTF); 
end; end;  if 1, hFeature=figure('Name', [ '3590' Afilemits]  ...
,'Position', [50 -100 scr(3)-100 scr(4)-100]); h=gcf; set(h,'WindowStyle','docked'); 
 set(gca, 'Units', 'normalized',   'FontUnits', 'points', ...
 'FontWeight','normal',  'FontSize', fonts16 , 'FontName','Times') ;
[ Pb2Rmax Pb2RmaxI ]= max ( AX(: , 6 ) )
DUR=Ob-AX(: ,6)'+1; [ DURv  DURi] =sort(DUR ,'descend');
[ DURzv] =  find(DUR<=0 ) 
 for k = 1  :  AAr1    
AAL2695( k ) = length(AXQRS{ k}) ; [ AAA2695IM(k ) ] =AX( k , 6) ;
%zs3l(k)=length([O-AAA2695IM(k)+1:O]);%zs3r(k)=length([1:AAA2695IM(k)]);
zsNUM0(k , Ob-AAA2695IM(k)+1 : Ob) = AXQRS{k ,1}(1: AAA2695IM(k) ) ;
% zsNUM0( k , 1 :  O- AAA2695IM(k)) =    Inf('double') ;
%  zsleft( k)  =length( [ O+1 :O+AAL2695(k) - AAA2695IM(k)]) ;
%  zsright( k) = length ([AAA2695IM(k)+1  :  AAL2695(k)  ]);
zsNUM0(k, Ob+1: Ob+AAL2695(k) -AAA2695IM(k)) =...
  AXQRS{k,1 }( AAA2695IM(k)+1: AAL2695(k) ) ;
i= AX( k ,3 ); if  i == 5  ||  i == 10 ,  c2='r' ;  
elseif  i == 1 ||  i == 2  ||  i == 3  ||  i == 11 ||  i == 34 ,    c2='g' ;  
elseif  i == 4  ||  i == 7 ||  i == 8  ||  i == 9 ,  c2='m' ; else   c2='y' ;      
 end;  plot( zsNUM0( k , : ) ,  c2 , 'LineWidth', 1 ) ;   hold on ; 
AA1NUMBC( k )= length (zsNUM0(k-0,: ) ) ;          
end; if 0,  for k = 1 :  AAr1 
AAL2695( k) =length( AXQRS{ k } ) ; [ AAA2695IM( k ) ] = AX( k , 6 ) ;
%if (Ob-AAA2695IM(k)+1)<= 0, znm0= znm0+1; zm0(znm0) = k ; end; 
if  AAA2695IM(k)-Ob> 0, znum0= znum0+1; zn0(znum0) = k ; 
z0(znum0) = Ob-AAA2695IM(k)+1 ;
zsNUM0(k, 1: Ob) = AXQRS{ k ,1}(  AAA2695IM(k)- Ob+1: AAA2695IM(k) ) ;
%zs3l(k)=length([O-AAA2695IM(k)+1:O]);zs3r(k)=length([1:AAA2695IM(k)]);
else  %if AAA2695IM(k)+1-Ob<= 0,znu0= znu0+1; zu0(znu0) = k;  end;
 zsNUM0(k ,Ob-AAA2695IM(k)+1 : Ob) = AXQRS{k ,1}( 1: AAA2695IM(k));
end; % zsNUM0( k , 1:  O-AAA2695IM(k)) = Inf('double'); R= AX( k  ,1 )  
%  zsleft( k) =length( [ O+1 :O+AAL2695(k) - AAA2695IM(k)]) ;
% zsright(k) = length ([AAA2695IM(k)+1 : AAL2695(k) ]); zlen1170( k)
zsNUM0( k, Ob+1: Ob+AAL2695(k) - AAA2695IM(k) ) = ...
AXQRS{ k,1 }( AAA2695IM(k)+1: AAL2695(k) ) ;
 if  0, plot( zsNUM0( k , : ), c2 , 'LineWidth', 1 );  hold on; end;
  AA1NUMBC(k) =  length (zsNUM0(k-0,: ) ) ;          
end; end;    if track&&DBUG dbstop 3626; end; 
L1s= num2str(L1,20 ); L2s= num2str(L2,'%4.0f');
 L3s= num2str(L3, 20);  L4s= num2str(L4,'% .1d') ; 
 strim1= ['Green      N#' L1s ]  ; strim2= ['Magenta  S#' L2s] ;
 strim3= ['Red         V#' L3s ];  strim4= ['Yellow      F#' L4s ];
 [ legh,oh,plh,ts ]=legend( strim1, strim2, strim3,strim4);
 FIGFIL =[ B  d(1:2), d(4:6), d(10:11), d(13:14), d(16:17) B];
[A11MAT A11MATI ] = max(AAL2695) ;A11NUM2CME= size(zsNUM0,2);
 cwidt=max(A11MAT, A11NUM2CME);  wxtick=[ 0  floor( (cwidt)/2) cwidt ] ; 
 Ym =min(zsNUM0(:,Ob))-0.2;Yx=max(zsNUM0(:,Ob))+0.2; 
 fyxdfc= [ Ym-0.2   0  Yx+0.2 ] ;  % fyxdfc= [  -2   0  3 ] ; 
if 10, for i=1: length(fyxdfc) [STfyxdfc{i,1},er] =sprintf('%.2f', fyxdfc(i)); end;  
 if fyxdfc(1)  <0 & fyxdfc(3)> 0 else  fyxdfc=  [ Ym-0.2   Yx+0.2 ] ;end; 
set(gca,'YTick',fyxdfc); set(gca,'YTickLabel',STfyxdfc);ylim([fyxdfc(1) fyxdfc(end)]); 
end;  set(gca,'XTick', wxtick); set(gca,'XTickLabel', wxtick); xlim([ 0  cwidt]);
title(['Record ' Afilemits ],'color','r','FontSize',16);cnptoqc=[Afilemits 'hFeature'];  
FIGUFILh =fullfile(PATFIG,[cnptoqc FIGFIL]) ; if DBUG, dbstop 3640; 
 saveas( hFeature , [ FIGUFILh Afilemits  B  'png' ] , 'png');  
end;end; end;  if 10,  if 1, [ zM zMI ]=max(AA1NUMBC )   
[ A1NUMBC  NUMBC] =sort( AA1NUMBC,'descend'); 
  [bnum, mnum, num] = unique(AA1NUMBC, 'first') ; mnum
A11MINE =min(AAL2695) ;  A12=260; Eror1=single(AX( zMI, 1:2));
[ Pb2Rmax Pb2RmaxI]=max(AX(:,6)); Pb2Rmean=mean(Pb2R);Pb2Rs=std(Pb2R);
[ RTemax  RTemaxI ]=max( RTe);[ RTemean] =mean(RTe); RTes=std(RTe); 
[Pb2Temax A1Pb2TemaxI]=max(Pb2Te);Pb2Temean=mean(Pb2Te);
[ RTesort  RTesortI ]=sort( RTe,'descend' );Pb2Tes=std(Pb2Te);
TWmin=min(TWO);TWmax=max(TWO);PWmin=min(PWO); PWmax=max(PWO); 
[ TWmaxL TWmaxI ]= max(FFTW ); wRU =[ PWmin PWmax  TWmin TWmax ] ; 
clear   A1F*  A1T* FF  F3  F4 FF* -except FFTW; time=toc
 wVZ=[  0 A11NUM2CME  A11MINE A11MAT  time ]; ds=datestr(now) ; 
waaae=[pefilter_param pethresh_param  pb_param pbslope_thresh pt_param]; 
 walap =[ 0   Pb2Rmax RTemax  A11NUM2CME 0   ];
datest=[ ds(10:11),ds(4:6), ds( 1:2)];  ptparamfile=[  'wused' datest ] ; 
 A1filexl2 = [ PATXL  ptparamfile B datest  '.xls' ] ; if  DBUG, dbstop 3654; end; 
end; if 10,    xlrowcol170105; 
  if 0, hnf_6s=close( h_940); 
if exist( 'h2AFL', 'var' ), h2AFLs=close( h2AFL); end;
if exist('hadcgain', 'var'),  hadcgains=close( hadcgain);  end;
 hpoffs= close( hpoff ); htons = close(hton); hl9s=close( h_9_PT); 
 end; if exist('hadc', 'var'),  h2s = close( hadc) ; end; 
  if exist('hrxdiff', 'var'),   hadcgains= close( hrxdiff) ;  end;
 if exist( 'hTOFF2', 'var' ), hTOFF2s  = close(  hTOFF2) ;end; 
%  if exist( 'hwan', 'var'), strlead1s= close( hwan );end; 
 if exist( 'hTOFF3', 'var' ), hbans = close( hTOFF3) ;   end; 
%  if exist( 'hband1', 'var' ), hband1s = close( hband1) ; end; 
% if exist( 'hband2', 'var' ), hband2s = close( hband2) ;  end; 
%  if exist( 'h2265', 'var' ), h2265s = close( h2265 ) ;  end; 
%  if exist( 'h1466bl', 'var' ),  h1466bls=close( h1466bl );   end;  
    HC=unique( ca ); hC=unique( CA); NHC= length( HC ); 
zlensort1820I=find(ca ==1 |  ca ==2 |  ca ==3 | ca ==11 | ca==34); 
 RPSTP=find( ca == 4  |  ca == 7 |  ca == 8 |  ca == 9); n=0;  m=0; 
ROCNPT= find(ca ==5 |  ca ==10); A1nsv_indicesTF=find(ca== 6);
L1 =length( zlensort1820I) ; L2= length( RPSTP) ; 
L3 = length( ROCNPT) ;  L4= length( A1nsv_indicesTF); 
Ym =min(zsNUM0(:,Ob))-0.2; Yx=max(zsNUM0(:,Ob))+0.2;  
  end; if Afilemit == 232;  CELL3y = [ -2 0 2];  
elseif Afilemit ==105,CELL3y =[-3 0 3];elseif Afilemit==200, CELL3y=[-3 0 3];
elseif Afilemit ==  201 ;  CELL3y =[ -1.3 0  2];   
 elseif Afilemit ==202 ;  CELL3y = [ -1.5 0  2.5];  
elseif Afilemit ==122 ;  CELL3y = [ -2 0  2];
elseif Afilemit ==124,CELL3y = [ -3 0 3]; elseif Afilemit ==219,CELL3y = [ -2 0 3];    
   elseif Afilemit ==  222,CELL3y = [ -2 0  2];   
elseif Afilemit ==  233,CELL3y =[ -3 0  3]; elseif Afilemit ==234,CELL3y= [ -2 0 2];   
  else CELL3y =[ -3 0  3]; 
  end;  if 1, htimeR =figure('Name', [ '3680 ' Afilemits ] ...
,'Position', [ 50  50  screen(3)-100 screen(4)-150]); 
 h =gcf;   set(h,'WindowStyle','docked'); htimeRs =figure( htimeR )
set( gca,  'Units', 'normalized',   'FontUnits', 'points', ...
 'FontWeight', 'normal',  'FontSize', fonts16 , 'FontName', 'Times');   
for l =1 : length( HC)  i = HC(l);  if  i == 5  ||  i ==10,  c='r' ;  
elseif  i == 1 ||  i == 2  ||  i == 3 ||  i == 11 ||  i == 34,  c='g' ;  
elseif  i == 4  ||  i == 7 ||  i == 8  ||  i == 9,  c='m' ; else   c='y' ;      
 end;   HC1 = find( ca == i ); YQP = zsNUM0( HC1, :) ;
 if i~= 12 & i~= 13 & i~= 38 
for j =1 :  length( HC1) line1cc{ l } = plot(YQP(j,: ), c ); hold on; end;
 end; end; if 1,   
for i=1: length(CELL3y) [STfyxdfc{i,1} ,err] =sprintf('%.2f', CELL3y(i)); end;  
set( gca,  'YTick', CELL3y ); set( gca, 'YTickLabel',  STfyxdfc) ; 
wxtick=[ 0  floor( (cwidt)/2)   cwidt ] ;  ylim([ CELL3y(1) CELL3y(end) ]) ;
set(gca,'XTick', wxtick); set(gca,'XTickLabel', wxtick); xlim([ 0  cwidt]);
title([ 'MIT-BIH Record  ' Afilemits ] ,'color', 'r' , 'FontSize' ,16 );
 [s1, errms] = sprintf('%5d',L1); [s3, errg] = sprintf('%8d',L3 );
[s2, errsg] = sprintf('%8d',L2 ); [s4, errm] = sprintf('%7d',L4 );
strm1= ['Green       N ' s1] ; strm2 =['Magenta   S ' s2];
strm3= ['Red          V ' s3];  strm4= ['Yellow       F ' s4];    
if 0,[ legh,oh,plh,ts ]=legend( strm1, strm2, strm3,strm4);
elseif 1, [ legh,oh,plh,ts ]=legend( strim1, strim2, strim3,strim4);% comment
 end; cnptoqc=[Afilemits  'htimeR']; hc=get( legh, 'children'); 
FIGFILE= [ cnptoqc  d(1:2), d(4:6), d(10:11), d(13:14), d(16:17) B] ;
FIGUFILE = fullfile(PATFIG, [  FIGFILE]); if DBUG,  %dbstop  3716;
saveas( htimeR , [ FIGUFILE Afilemits  B  'png' ] , 'png')  ;  
 end; end; end;  if 1,hFeatMap =figure('Name', [ 'Map3720 ' Afilemits]...
,'Position', [ 2 10  screen(3)-100 400 ]); h =gcf ;set(h,'WindowStyle','docked') ; 
set( gca,  'Units', 'normalized',   'FontUnits', 'points', 'FontName', 'Times',...
 'FontWeight', 'normal',  'FontSize', fonts16 ); mHC=0; NR=[];
wxtick= [ 0  floor( (cwidt+0)/2)  cwidt] ; set(gca,  'XTick', wxtick);  
set(gca,'XTickLabel',wxtick ); xlim([ 000  cwidt ]) ; if  DBUG, dbstop 4005; end; 
for l = 1 :  NHC    i =  HC(l );    trac =  l ;  
 if  i == 4  ||  i == 7 || i == 8  || i == 9,  c='m';  
elseif  i == 1 ||  i == 2  ||  i == 3  ||  i == 11 ||  i == 34 , c='g' ;  
elseif  i == 5 || i == 10,  c='r' ;  elseif  i == 6 ,  c=cb;  else   c=cy ;      
end;  mHC=mHC+1; Cf = subplot( NHC, 1, mHC) ;  
HC1 =find( ca ==i ); YQP = zsNUM0( HC1, :) ; L= int2str( length( HC1));  
for j = 1 : length(HC1)  Nanrows(trac)=plot(YQP(j, : ) , c); hold on; end;
tra= l ; NR = [NR;  Nanrows(tra)] ;xlim([ 0  cwidt ]) ;
 set( gca,  'Units', 'normalized',   'FontUnits', 'points', ...
 'FontWeight', 'normal',  'FontSize', fonts16 , 'FontName', 'Times' ) ; 
title([ L ' '  CLASS{  i } ] ,'color', 'r' , 'FontSize' ,16 ); 
title( Cf , [   CLASS{ i} ] ,'color', 'r' , 'FontSize' ,16 );
[ linecells{ l } , cT2 ]=  sprintf('%-20s', CLASS{i}) ; 
 linec{ l ,: }= [ num2str( L,20)  ' '  CLASS{ (i) } ];  
 linecell{ l ,:}= [ CLASS{ i }  ' ' num2str( L ,10)];  
 set( gca,  'YTick', CELL3y) ;
 for i= 1: length( CELL3y ) [ STfyxdfc{ i,1} , err] = sprintf('%.1f', CELL3y(i) ); end; 
  set( gca, 'YTickLabel',  STfyxdfc ) ;  ylim([ CELL3y(1)  CELL3y(end)]);  
 wxtick=[ 0 : floor( (cwidt+00)/2): cwidt+00] ; set(gca,  'XTick', wxtick); 
 set(gca,'XTickLabel', wxtick );  xlim([ 00  cwidt+00 ]) ;
end;  if 0, hL = legend(Nanrows, linec' );
end;  if 0, legen2 ={  'N' ,'S' ,'V','F','Q','LC6','LC7' }; 
hlegTT =legend(linec' ); hL1 = legend( linecells,  { 'N 1' ,'S 2'  ,'V 3' ,'F 4' }); 
hL3 = legend([line1,line2,line3,line4],{' Data 1','Data  2','Data  3','Data  4'});
end; if 0, newPosition = [0.7 0.7 0.4 0.4];   newUnits = 'normalized';
set(hL , 'Position', newPosition, 'Units', newUnits);
end;  cnptoqc= ['hFeatMap' Afilemits ] ; hold off; 
FIGFILE= [ cnptoqc  d(1:2), d(4:6), d(10:11), d(13:14), d(16:17) B] ;
FIGUFILE = fullfile(PATFIG, FIGFILE);
hFeatMaps =figure(hFeatMap); if DBUG, 
 saveas( hFeatMap , [ FIGUFILE Afilemits  B  'png' ] , 'png');  
end;end;  if 10, hFeaAAMI =figure('Name', ['3755 AAMI ' Afilemits]...
,'Position', [ 10 10  screen(3)-100  screen(4)-100])  ;
h = gcf ;  set(h,'WindowStyle','docked') ; NanGPRL
set( gca,  'Units', 'normalized',   'FontUnits', 'points', ...
 'FontWeight', 'normal',  'FontSize', fonts16 , 'FontName', 'Times' ) ; 
zlensort1820I= find(ca == 1 |  ca == 2 |  ca == 3 |  ca == 11 |  ca ==34 ); 
 RPSTP=find( ca == 4  |  ca == 7 |  ca == 8  |  ca == 9 ); n=0;  m=0; 
ROCNPT= find(ca == 5 |  ca ==10); A1nsv_indicesTF= find(ca == 6);
L1=length( zlensort1820I) ;L1s= num2str(L1);
 L2= length(RPSTP) ; L2s= num2str(L2);
L3 = length( ROCNPT);L3s= num2str(L3);
 L4= length( A1nsv_indicesTF); L4s= num2str(L4);
n= 0; if  L1,  n=n+1; end;   if  ~isempty( RPSTP), n=n+1;  end;
if ~isempty(ROCNPT),n=n+1;end; if ~isempty(A1nsv_indicesTF),n=n+1; end;
for i=1: length(CELL3y) [ST3adc{ i,1}, cT2]=sprintf('%5.1f',CELL3y(i)); end;
line1=0;  line2=0;    line3=0;   line4=0;  hFeaAAMIs =figure(hFeaAAMI ) 
if L1, m=m+1;   top1=  subplot( n, 1, m) ;
 YQP1 = zsNUM0( zlensort1820I, :) ;
 for j =1: length( zlensort1820I),  line1=plot(YQP1( j, : ), cg); hold on; end;
 set( gca,  'Units', 'normalized',   'FontUnits', 'points', ...
 'FontWeight', 'normal',  'FontSize', fonts16 , 'FontName', 'Times' ) ;  
wxtick= [ 0  floor((cwidt+0)/2)  cwidt ] ;  xlim([ 000  cwidt ]) ;
 set(gca,  'XTick', wxtick   );  set(gca,'XTickLabel', wxtick );
 title2 =[ '  Q R S detection Record  ', Afilemits ];   
  set(gca,'YTick', CELL3y ) ;   axis([ -0  cwidt  -3  4 ]);  
   set(gca, 'YTickLabel',  ST3adc ) ; ylim([ CELL3y(1)    CELL3y(end)]) ;
 title( [  ' AAMI1 ' num2str( L1) ] ,'color', 'r' , 'FontSize' ,16 ); 
if 0, ylabel( 'Amplitude, mV'  ,   'FontSize',  16  , 'Color', 'b'  );
xlabel( 'Time in samples',  'Color' , 'b' , 'FontSize' , 16 );
end;end; if L2, m=m+1;top2= subplot( n, 1, m) ; 
YQP2 = zsNUM0( RPSTP, :) ;
 for j = 1: length( RPSTP)  line2=plot( YQP2(j, : ) , cm ) ;  hold on ; end;
 set( gca,  'Units', 'normalized',   'FontUnits', 'points', ...
 'FontWeight', 'normal',  'FontSize', fonts16 , 'FontName', 'Times' ) ;  
 set(gca,'YTick', CELL3y ); set(gca, 'YTickLabel',  ST3adc ) ; 
set(gca,  'XTick', wxtick   );  set(gca,'XTickLabel', wxtick );
title([ ' AAMI2 ' num2str(L2) ] ,'color', 'r' , 'FontSize' ,16 ); 
 xlim([ 000  cwidt ]) ;ylim([ CELL3y(1)   CELL3y(end)]) ; 
 if 0, ylabel( 'Amplitude, mV'  ,   'FontSize',  16  , 'Color', 'b'  );
xlabel( 'Time in samples',  'Color' , 'b' , 'FontSize' , 16 );
 end;
end; if L3, m=m+1;  top3= subplot( n, 1, m) ;
 YQP3 = zsNUM0( ROCNPT, :) ;%axis([ -00  cwidt  -3  4 ]);  
  HF3T = AX( ROCNPT, : ) ;
 for j = 1:   length( ROCNPT) line3=     plot( YQP3(j, : ) , cr ) ;  hold on ; 
  if 0, str3537{j} =int2str(HF3T( j, 35)) ; str3733{j} =int2str(HF3T(j ,1)) ;    
 [TT3750A(j)  TT3750AI(j ) ]=min(YQP3(j , : ) );
% text( O  , YQP2(j , O ) ,  [ str3537{j}  str3733{j} ]   ,'color', 'r' );
 text( TT3750AI(j)  , TT3750A(j) ,  [ str3537{j} str3733{j} ] ,'color',c2 ); 
elseif 0,  [TT2750A(j)  TT2750AI(j ) ]   = max( YQP3(j , : ) ) ;
 text( TT2750AI(j) ,TT2750A(j) ,  [ str3537{j}  str3733{j} ] ,'color', c2); 
else  [AAA(j) AAA2714I(j) ]=max(YQP3(j ,: ));%[TT2750AI(j)]=(HF3T(j,6));
%  text( TT2750AI(j) , AAA(j) , [  'i='   int2str(j) ] ,'color', 'k' );  
 end; end; set( gca,  'Units', 'normalized',   'FontUnits', 'points', ...
 'FontWeight', 'normal',  'FontSize', fonts16 , 'FontName', 'Times') ;  
   set(gca,'YTick', CELL3y ) ;set(gca, 'YTickLabel',  ST3adc ) ; 
 set(gca,  'XTick', wxtick   );  set(gca,'XTickLabel', wxtick );
 xlim([ 00  cwidt ]) ;ylim([ CELL3y(1)    CELL3y(end)]) ;
title(['AAMI3 ' num2str(L3)] ,'color', 'r','FontSize',16);%axis([-0 cwidt  -3 4]);  
if 0, ylabel( 'Amplitude, mV'  ,   'FontSize',  16  , 'Color', 'b'  );
xlabel( 'Time in samples',  'Color' , 'b' , 'FontSize' , 16 );
 end;
 end; if L4, m=m+1; top4= subplot( n, 1, m) ; 
YQP4 = zsNUM0( A1nsv_indicesTF, :) ;%fusion
 for j =1: length(A1nsv_indicesTF)  line4= plot( YQP4(j,:), cb);hold on; end;
 set( gca,  'Units', 'normalized',   'FontUnits', 'points', ...
 'FontWeight', 'normal',  'FontSize', fonts16 , 'FontName', 'Times' ) ;  
 set(gca,'YTick', CELL3y ) ; set(gca, 'YTickLabel',  ST3adc ) ; 
set(gca,  'XTick', wxtick  );  set(gca,'XTickLabel', wxtick );
 ylim([ CELL3y(1)    CELL3y(end) ]) ;  xlim([ 000  cwidt ]) ;
title([  ' AAMI4 ' num2str(L4) ] ,'color', 'r' , 'FontSize' ,16 );  hold off; 
if 0, ylabel( 'Amplitude, mV'  ,   'FontSize',  16  , 'Color', 'b'  );
xlabel( 'Time in samples',  'Color' , 'b' , 'FontSize' , 16 );
 end;end;   if 0, hLA = legend( [line1,   line2,  line3,line4] , ...
{['N ' num2str(L1,10)] ,['S ' num2str(L2,10)] ,...
[ 'V ' num2str(L3,10) ],[ 'F ' num2str(L4,10)]}); 
newPosition = [ 0.7 0.7 0.23 0.23];  newUnits = 'normalized';
set( hLA , 'Position',  newPosition, 'Units', newUnits);
  end; [ax2,h2]=suplabel('Amplitude, mV','y');
  set(h2,'FontSize',16,  'Color' , 'b'  )
  cnptoqc= [  'hFeaAAMI' Afilemits ];%h_145 = figure(hFeature) ;  
 FIGaami= [ cnptoqc '4'   d(1:2), d(4:6), d(10:11), d(13:14), d(16:17) B] ;
FIGUFILE = fullfile(PATFIG, FIGaami);  if DBUG,  dbstop  3833; 
 saveas( hFeaAAMI , [ FIGUFILE     'png'  ] , 'png') ;  
  end; clear  ANNtime CLL C_AB YQP*  cl cla  uRO uROI  zsNUM0 CF; 
 iqrs= { 'FontSize', 14 } ; AVr1= num2str(AAr1);

  end; if  0,  for k = 1 :  AAr1 
  zsNUM1( k , Ob-AAA2695IM(k)+1 : Ob) = ...
AXQRS{k  ,1 }( 1: AAA2695IM(k) ) ;
zsNUM1( k, Ob+1: Ob+AAL2695(k) - AAA2695IM(k) ) =...
AXQRS{k,1 }( AAA2695IM(k)+1: AAL2695(k) ) ;
plot( zsNUM1( k , : )  ,  c2  , 'LineWidth', 1 ) ;  hold on ;
 end; end; if 10, if  ~isempty( zn0 ), if DBUG   dbstop 3838; end; 
[ mzo  mz0I ] = min( z0); zn0mink= zn0(mz0I ); zn0minkR=Ra(zn0(mz0I)) ;
[ z0val, z0ind] =sort(z0); z0minks=zn0(z0ind ); 
 z0minksR=[Ra( z0minks)'] ;  Rz = AX( zn0mink ,1 ); Rz~=zn0minkR
  FIGFIL= [ ds(1:2), ds(4:6), ds(10:11), ds(13:14), ds(16:17) ];    
 A1filename = [ PATFIG Afilemits FIGFIL  '.txt'] 
 dlmwrite( [A1filename ] , zn0 , 'delimiter', '\t' ,  'precision', 10 ) ;  
if  ~isempty( zn0), R1000= [ z0minksR ]; Ritmax3= length(R1000); end; 
 %error( '4305 error') ; return ;  %thesis
end; fprintf(' 3820 Reprod%s\n', uC ); zzlen
if size( R1000,1) ~=1,  R1000 = [  R1000' ]  ; end;  TIM= [ '' ] ;
if  length(R1000)>3, Ritmax3=3; else Ritmax3=length(R1000); end; 
if  length(R1000)>=1, Ritmax3=length(R1000); 
else  R1000=Ra( zlensort1170I( hwan+1)); Ritmax3= length(R1000);     
end; end;end; end; if 1, fprintf(' 3850 Reprod%s\n',uC ); zzlen
if Afilemit ==201, R1000 = [Ra(1528) Ra(704) Ra(609) Ra(449) ...
 Ra(561) Ra(624) Ra(1634)];    
 elseif Afilemit == 121, R3000 =  [  Ra( 162)  Ra(1017 )  Ra( 86 )  R1000 ]; 
  elseif Afilemit == 202, R3000 =  [  Ra( 745 )  Ra( 1341)  Ra( 356 ) R1000 ];   
elseif Afilemit == 207, R3000 =  [  Ra( 1646)  Ra(1647 )  Ra( 86 )  R1000 ]; 
 elseif Afilemit == 222, R1000 = [  Ra( 673)  Ra( 675 )  Ra( 801)  Ra(2128) ];    
elseif Afilemit == 223, R1000 =  [  Ra( 162)  Ra(1307)  Ra( 86 )  R1000 ];     
end; R3000= unique([Ra(A1Pb2TemaxI)  R1000]); 
 Ritmax3=length( unique( R3000)) ;PeSQ=1;  %thesis
for  J = 1 : PeSQ*1    if 1,  
f =find(Ra ==R3000(J) ) ;  IR =find( RO == R3000(J));  
if  length(f) >1, f1= f(1);  f2=f(2 );  I1 = IR(1); I2 = IR(2);
 U =Ra( f1-2) - 20 ;  V = Ra( f2 +2) + 50 ;  f= f(1) ;  IR =  IR(1 );
elseif  length(f)==1,if f+1<=length(Ra)&&(f-1)>1,U=Ra(f-1)-70;V=Ra(f+1)+80; 
 else continue; end; else continue;  
  end;  if Afilemit == 205&& f ==2189,U = Ra(f-1)-50; V=Ra(f +1)+50; 
elseif Afilemit == 207 && f==238,U = Ra( f  ) - 320 ; V = Ra( f )+320;      
elseif Afilemit == 207 && f==1645,U = Ra( f  ) - 120 ; V = Ra( f )+20;    
elseif 0&&Afilemit == 200&& f==2229,U =Ra( f-1)-20; V = Ra( f +1)-20;     
  end;  if ~isempty( zn0 ),  ff =find( z0minksR == R3000(J) ) ; 
end;F3=  [  'Reprod  ' 'Beat ' int2str(f) 'Beat R' int2str(R3000(J)) ] ;
if ~isempty( zn0 ),  ff  =find( z0minksR == R3000(J) ) ; 
if ff, F3=  [  'z0 Reprod  ' 'Beat ' int2str(f) 'Beat R' int2str(R3000(J)) ];
end; end; end ; if 1,h_940re(J) = figure('Name',  ['3890 '  Afilemits F3 ]  ...
,'Position', [ 10+J  10 screen(3)-10  screen(4)-100]);m=0; K2= f;
 h =gcf ;  set( h,'WindowStyle','docked') ; 
 if  (U<=0), U=10; end; if  (V >= NS), V =NS -10;end;     
if Afilemit==205&&f==2189, fs=int2str(f); fsm1=int2str(f-1); fsp1=int2str(f+1);
else  fs= [] ; fsm1=  [] ; fsp1= [] ;
end; if 1,if 1,N=2 ; m=1;  hsrep1= subplot( N ,1,m );
plot( U : V , xts( U : V ) , 'm'  , 'LineWidth', 2 );  Ma=0.1;
if BL,  line( [ U, V] , [0, 0] , 'Marker', '.'  , 'Color', 'k' ); 
elseif Afilemit==213,  line( [ U, V] , [0, 0] , 'Marker', '.'  , 'Color', 'k' );   
 elseif 1,  line([ U, V], [ 0,0 ] , 'Marker' , '.', 'Color', 'k' , 'LineWidth', 1);        
end; if Afilemit==213,  Mr=0.21; mr=0.21; else Mr=0.1; mr=0.1; 
 end;    minxbl1=min( xts( U :V ))-mr ;  maxxbl1=  max( xts( U : V ))+Mr ;    
 minxbf2uv= min([xts(U : V)] )-mr; maxxbf2uv =max([ xts( U: V)])+Mr;
 set(  gca,  'Units','normalized','FontUnits', 'points', 'FontWeight',  'normal', ...
  'FontSize', fonts16 , 'FontName', 'Times'); axis([ U V  minxbf2uv  maxxbf2uv ]); 
set(gca,'XTick', U:fix((V-U)/2):V); set(gca, 'XTickLabel', (get(gca, 'XTick')));
CELL3y = minxbf2uv:(maxxbf2uv- minxbf2uv)/2 : maxxbf2uv;  
for i= 1: length(CELL3y) [ST1adc{ i,1}, celT2] = sprintf('%5.2f',CELL3y(i)); end;
  set(gca,    'YTick', CELL3y); set(gca,  'YTickLabel', ST1adc); 
  title(['Laguna ecgpuwave Software, class ' Ca(f) ', Beat ' ...
      int2str(f) ', Record ' Afilemits] ...
 ,'color', 'r', 'FontSize' ,16 ); CL60=int2str(Pb2O(f));Pb2MnanN=int2str(Ra(f-1))
 ym= maxxbf2uv-Ma ; y=maxxbf2uv-Ma; yp=maxxbf2uv-Ma ; 
  if Afilemit==117,  zerr43fm1= 0.2; zerr43f= 0.2;  zerr43fp1= 0.2;
elseif Afilemit== 113,zerr43fm1=zerrA1(f-1); zerr43f= zerrw1(f);zerr43fp1=zerrf1(f+1);
elseif Afilemit==201,zerr43fm1= zerrA1(f-1); zerr43f= zerrw1(f);zerr43fp1=zerrf1(f+1) ;
elseif Afilemit== 208 & f==1770  ,
    zerr43fm1= zerrA1(f-1); zerr43f= zerrA1(f );   zerr43fp1= zerrf1(f+1);
elseif Afilemit==208,zerr43fm1=zerrA1(f-1); zerr43f= zerrA1(f-1);zerr43fp1=zerrf1(f+1);
elseif Afilemit== 220, zerr43fm1= zerrA1(f-1); zerr43f=zerrw1(f);zerr43fp1=zerrf1(f+1);
elseif Afilemit==222,zerr43fm1=zerrA1(f-1);zerr43f= zerrA1(f-1);zerr43fp1=zerrf1(f+1);
elseif Afilemit== 223&&f==306,,
  zerr43fm1= zerrA1(f-1) ; zerr43f= zerrf1(f) ;zerr43fp1=zerrf1(f+1) ;
elseif Afilemit==234, zerr43fm1=zerrA1(f-1); zerr43f=zerrw1(f);zerr43fp1=zerrf1(f+1);
else   zerr43fm1= zerrA1(f-1) ; zerr43f= zerrw1(f);zerr43fp1= zerrf1(f+1) ;  
end; CL=[ int2str(f) Ca(f) int2str(Ra(f ))];  if DBUG, dbstop 3921; end; 
  
    end; if (Afilemit==207 &f==1771 ) 
   text( Ra(f )-2 , xts(Ra(f))+zerr43f  , [  CL   ] ,'color', cb, 'FontSize',14) ;
   text(Ra(f-1), xts(Ra(f-1))+zerr43fm1 , [ int2str(f-1) Ca(f-1) ],'color', cr,'FontSize',14);
 text(Ra(f+1)-3,xts(Ra(f+1))-zerr43fp1, [ int2str(f+1) Ca(f+1)] ,'color',cr,'FontSize',14);  
elseif (Afilemit== 201 ) || ((Afilemit== 100 ) &f==1907) ,      
     zerr43fm1= 0 ; zerr43f= 0 ;zerr43fp1= 0 ;
text(Ra(f)+0,xts(Ra(f))+zerr43f ,[int2str(f) Ca(f) int2str(Ra(f))],'color',cr,'FontSize',15);
    text(Ra(f-1)+0, xts(Ra(f-1))+zerr43fm1, ...
        [ int2str(f-1) Ca(f-1) ], 'color',cr,'FontSize',15 );
    text(Ra(f+1)-3, xts(Ra(f+1))+zerr43fp1 ,  ...
        [ int2str(f+1) Ca(f+1)  ],'color',cr ,'FontSize',15);

elseif (Afilemit==100)|| ((Afilemit== 202 ) &f==356) ,   
ym= maxxbf2uv-Ma ; y=maxxbf2uv-Ma; yp=maxxbf2uv-Ma ; 
   text( Ra(f )-2 , y  , [  CL   ] ,'color', cb, 'FontSize',14) ;
   text(Ra(f-1), ym , [ int2str(f-1) Ca(f-1) ]  ,'color', cr,'FontSize',14);
 text(Ra(f+1)-3, yp, [ int2str(f+1) Ca(f+1)] ,'color',cr,'FontSize', 14);      
elseif (Afilemit== 223),
text(Ra(f), xts(Ra(f))+0,[int2str(f) Ca(f ) int2str(Ra(f))], 'color',cr,'FontSize',15);
text(Ra(f-1)+0, xts(Ra(f-1))+zerr43fm1, ...
 [ int2str(f-1) Ca(f-1) int2str(Ra(f-1))],'color',cr,'FontSize',15 );
text(Ra(f+1)-3,xts(Ra(f+1))+zerr43fp1,[int2str(f+1) Ca(f+1)],'color',cr,'FontSize',15);
 elseif (Afilemit==208 &f==1771 ) 
   text( Ra(f )-2 , xts(Ra(f))+zerr43f , [  CL ] ,'color', cb, 'FontSize',14) ;
   text(Ra(f-1),xts(Ra(f-1))+zerr43fm1,[ int2str(f-1) Ca(f-1) ],'color', cr,'FontSize',14);
text(Ra(f+1)-3,xts(Ra(f+1))-zerr43fp1, [ int2str(f+1) Ca(f+1)] ,'color',cr,'FontSize',14);   
elseif 1 ,  xtsRaf= xts(Ra(f));xtsRafm= xts(Ra(f-1)) ; xtsRafp=xts(Ra(f+1));
   text( Ra(f )-2 , xtsRaf     , [  CL   ] ,'color', cb, 'FontSize',14) ;
   text(Ra(f-1), xtsRafm  , [ int2str(f-1) Ca(f-1) ]  ,'color', cr,'FontSize',14);
 text(Ra(f+1)-3, xtsRafp , [ int2str(f+1) Ca(f+1)] ,'color',cr,'FontSize', 14);   
 
 end; if exist('Pb2M', 'var') && (~isnan(Pb2M(f))) && (f<= length(Pb2M))
 text( Pb2M(f) , xts( Pb2M(f) ),  [  'Pb' ] , 'color', 'b' , 'FontSize', 14);
   elseif 0, text( Pb2(f ) ,  xts( Pb2( f ) ),  [ fs  'NANPb' ] , 'color', 'b' , 'FontSize',14);
  end; if  exist('Pe2fM', 'var') &&  (~isnan (Pe2fM( f ) ))&&(f<= length(Pe2fM))
    text( Pe2fM( f ) , xts( Pe2fM( f) ),  [  'Pe' ] , 'color', 'b', 'FontSize', 14);
elseif 0,  text(Pe2f( f) ,  xts( Pe2f( f  ) ),  [ fs  'NANPe'  ] , 'color', cb, 'FontSize',14);
   end;  if  exist('Tb1fM', 'var') &&~isnan(Tb1fM(K2) ) &&( f<= length(Tb1fM) )
    text(Tb1fM( K2) , xts( Tb1fM( K2) ),[   'Tb'   ] , 'color', 'b', 'FontSize', 14 );
   elseif 0, text( Tb1fO( f ) ,  xts( Tb1fO(f) ),  [  'NANTb' ] , 'color', 'b', 'FontSize',14);
end; if  exist('Tef1M', 'var') && (~isnan(Tef1M( f )) ) && ( f<= length(Tef1M))
    text( Tef1M( f ) , xts( Tef1M( f) ), [    'Te'  ] , 'color', 'b' , 'FontSize', 14);
elseif 0,    text( Tef1O( f ) , xts( Tef1O( f) ),[  'NANTe'  ] , 'color', 'b' , 'FontSize',14);
end;  if  DBUG,  if IR+1<=length(RO) &&  RO(IR +1)~= Ra(f+1)  ,
text( RO(IR +1), xts(RO(IR +1))-0.1  ...
  , ['R' int2str(IR+1) C(IR +1) int2str(RO(IR +1))],  'color','r' , 'FontSize', 14 );
end;  if  IR-1>=1 &&  RO(IR-1)~=Ra(f-1),  
    text(RO(IR-1), xts(RO(IR -1))-0.1 ,   ...
[ 'R' int2str(IR-1) C(IR-1) int2str(RO(IR -1))],'color','r' , 'FontSize', 14  );
end; if 0&&(IR-2>=1&&RO(IR -2)~=Ra(f-2)), text((RO(IR -2)),xts(RO(IR-2))-0.1,...
 [ 'R'  int2str(IR -2)    C(IR -2) int2str(RO(IR -2))],'color','r', 'FontSize', 14);
end; end;  if 0 , text(S(f ), xts(S(f )) ,'S' , 'color', 'r' , 'FontSize', 14 ); 
 if exist('Rp','var') &&(Rp(f )>0) &&(~isempty(Rp)),
 text(Rp(f ), xts(Rp(f )) , 'rp', 'color', 'r' , 'FontSize', 14);  end;end;
if ~Bthesis,if exist('Tb1fM','var')&&~isnan(Tb1fM(K2-1))&&(f<=length(Tb1fM))
 text(Tb1fM(K2-1) , xts( Tb1fM( K2-1) ), [ fsm1  'Tb'] , 'color', cr , 'FontSize', 14 );
elseif 0,    text( Tb1fO( f-1 ) ,  xts( Tb1fO( f-1) ),  [ fsm1 'NANTb' ] , 'color', cr );
 end; if  exist('Tef1M', 'var') && (~isnan(Tef1M(f-1))) && (f<=length(Tef1M))
text( Tef1M( f -1) , xts( Tef1M( f-1) ), [ fsm1 'Te' ] , 'color', cr  , 'FontSize', 14);
 elseif 0, text(Tef1O(f-1), xts( Tef1O( f-1)),[ fsm1 'NANTe'] ,'color', 'r', 'FontSize',14);      
   end;  if exist('Pb2M', 'var') &&(~isnan(Pb2M( f+1)))&& (f+1 <= length(Pb2M))
    text( Pb2M( f +1) , xts( Pb2M( f+1) ),  [  fsp1  'PbM' ] , 'color', cr, 'FontSize', 14 );
elseif 0, text( Pb2( f+1),  xts( Pb2( f +1) ),  [fsp1 'NANPb'] ,'color',cr, 'FontSize',14);
end; if exist('Pe2fM', 'var') &&(~isnan(Pe2fM( f+1 ) ))&&(f<=length(Pe2fM))
 text( Pe2fM( f+1) , xts( Pe2fM( f+1) ),  [ fsp1 'PeM' ] , 'color', cr , 'FontSize', 14);
elseif 0,  text(Pe2f(f +1) ,xts(Pe2f( f +1)),  [fsp1 'NANPe' ] , 'color',cr, 'FontSize',14);
end;end; end; if 2,  m=m+1;  hsrep2= subplot( N ,1,m ); 
%  h_940reps2(J) = figure(h_940re(J) )
  plot( U : V, xts( U : V) , 'g'  , 'LineWidth', 2 ); hold on; 
%  minxbl1=min( xts( U :V ))-0.1 ; maxxbl1=  max( xts( U : V ))+0.1 ;    
 set(gca, 'Units','normalized',   'FontUnits','points', 'XTick', U:fix((V-U)/2):V, ...
 'FontWeight',  'normal', 'FontSize', fonts16, 'FontName','Times');
 set(gca, 'XTickLabel', (U:fix((V-U)/2):V) ) ;   xlim([U  V]) ;   
CELL3y =[ minxbl1:(maxxbl1- minxbl1)/2: maxxbl1  ];
for i=1 : length(CELL3y) [ST1adc{ i,1}, celB2] =sprintf('%5.2f',CELL3y(i)); end;
set( gca, 'YTick', CELL3y);   set(gca, 'YTickLabel', ST1adc);  
A1= [ ' Beat ' int2str(f)  ', Record '   Afilemits  ]; 
 title(['Tan''s Slope Algorithm for P and T detection' A1] ,'color', 'r' ,'FontSize',16);  
line([ U, V] , [0, 0] , 'Marker', '.'  , 'Color', 'k' );  ylim([ minxbl1  maxxbl1 ]) ; 

zlensort2 =   [ int2str(f-1) Ca(f-1)  int2str(Ra(f-1)) ] ; iqrs= { 'FontSize',14 } ;
zlensort1820 = [ int2str(f+1) Ca(f+1) int2str(Ra(f +1))] ; 
xtsRaf=xts(Ra(f )) ; xtsRafm=xts(Ra(f-1)) ; xtsRafp=xts(Ra(f+1)); 
F4= [ int2str(f) Ca(f) int2str(Ra(f)) ] ; if 1, dbstop 4005; end;
if ( Afilemit == 234&  f==1298 ) , 
 text( Ra(f), xts(Ra(f  ) )- zerrA2(f  ) ,F4 , 'color',  'r', 'FontSize', 14 );
text( Ra(f-1),xts(Ra(f-1))+zerrA2(f-1), [ int2str(f-1) Ca(f-1) ],'color','r' ,'FontSize',14);
text(Ra(f+1)-3,xts(Ra(f+1))+zerrf2(f+1), [int2str(f+1) Ca(f+1)],'color',cr,'FontSize',14);
 elseif  ( Afilemit == 208)  ...   
         
 text( Ra(f), 0+ maxxbl1-Ma ,F4 , 'color',  'r', 'FontSize', 14 );
  text( Ra(f-1),0+ maxxbl1-Ma, [ int2str(f-1) Ca(f-1) ],'color','r' ,'FontSize', 14);
text(Ra(f+1)-3, 0+maxxbl1-Ma,[ int2str(f+1) Ca(f+1)], 'color',cr ,'FontSize',14);
  elseif  ( Afilemit == 201)  ||  ( Afilemit == 202)  ||  ( Afilemit == 207)   ...   
          
    text( Ra(f), xts( Ra(f ) )+ 0 ,F4 , 'color',  cb , 'FontSize', 14 );
  text( Ra(f-1),xts(Ra(f-1))+0, [ int2str(f-1) Ca(f-1)   ],'color','r' ,'FontSize', 14);
text(Ra(f+1)-3, xts(Ra(f+1))+0 , [ int2str(f+1) Ca(f+1)] , 'color',cr , 'FontSize', 14);
else
    
    
    
end; if Bthesis&&DBUG, if IR+1<=length(RO)&&RO(IR+1)~=Ra(f+1) , 
text(RO(IR+1),xts(RO(IR+1)),[int2str(IR+1) C(IR+1) int2str(RO(IR+1))]...
 ,'color',cr,'FontSize',14);
end; if (IR-1>=1)&& RO(IR -1 )~=Ra(f-1), text(RO(IR -1),xts(RO(IR -1))-0.1, ...
    [ 'R' int2str(IR-1) C(IR-1) int2str(RO(IR-1))],'color','r' , 'FontSize', 14  );
 end;   if 0&&( IR-2 >=1) , text((RO(IR -2)),xts(RO(IR-2))-0.1 ,...
    [ 'R'  int2str(IR -2)    C(IR -2) int2str(RO(IR -2))],'color','r', 'FontSize', 14);
end; end; if 1,  if ( Pb2O(f)> 0)  && (Pe2fO(f)-Pb2O(f)  ) >20 , %thesis 
text( Pb2O( f) , xts(Pb2O( f) ),  [   'Pb'] , 'color', 'b' ,  'FontSize', 14 ); 
end;  if ( Pe2fO(f)>0) && (Pe2fO(f)-Pb2O(f)  ) >10 ,
    text(Pe2fO( f) , xts( Pe2fO(f) ),[ 'Pe' ] , 'color', 'b' , 'FontSize', 14  );
end;if (Tb1fO(K2)>0),text(Tb1fO(K2), xts(Tb1fO(K2)),['Tb'], 'color',cb,'FontSize',14);
 end; if (Tef1O(K2)>0),
 text(Tef1O(K2), xts(Tef1O(K2)), [ 'Te' ] ,'color', 'b', 'FontSize',14);
 end; if Bthesis, if (Tef1O(K2-1)-Tb1fO(K2-1)>10),
    text(Tb1fO(K2-1) , xts(Tb1fO( K2-1)), [ 'Tb' ] , 'color', cr , 'FontSize', 14 );
if (Tef1O(K2-1)>0),
    text(Tef1O(K2-1),xts(Tef1O(K2-1)), ['Te'] ,'color',cr, 'FontSize',14);
 end;  end; if Pb2O(f+1)&& (Pe2fO(f+1)-  Pb2O(f+1) ) >1 , 
     text(Pb2O(f+1),xts(Pb2O(f+1)), [  'Pb'] ,'color', 'r', 'FontSize',14); 
 if (Pe2fO(f+1)), text(Pe2fO(f+1), xts(Pe2fO(f+1)),['Pe'] ,'color', cr ,'FontSize',14);
end; end; end;end; end; if N >2,  m=m+1 ;  hsrep3=subplot( N,1,m);  
plot( U: V , xts( U : V) , 'r', 'LineWidth', 2 );
 line( [ U, V] , [0, 0] , 'Marker', '.'  , 'Color', 'k'  );  
 [ minxbf1uv ] =min(xts(U : V));  [ maxxbf1uv ] =max(xts(U : V));  
 axis([ U V  minxbf1uv-0.01 maxxbf1uv+0.01]); 
 hold on;  set( gca, 'Units' , 'normalized', 'FontUnits','points', ... 
 'XTick', U:fix((V-U)/2):V,'FontUnits', 'points', ......
 'FontWeight', 'normal', 'FontSize', fonts16, 'FontName', 'Times');
 ROM =  [ minxbf1uv:(maxxbf1uv- minxbf1uv)/2 : maxxbf1uv ] ;     
 for i= 1: length( ROM) [ ST1{ i,1} , errmsg] = sprintf('%.2f',  ROM(i) ) ;
end; set( gca,  'YTickLabel',  ST1 )  ;   
 set( gca, 'YTick', minxbf1uv:(maxxbf1uv- minxbf1uv)/2:maxxbf1uv,...
'XTick', U:fix((V-U)/2):V ) ; %  set(gca, 'XTickLabel', num2str(get(gca,'XTick'))) ;
set(gca,  'XTickLabel', (get(gca, 'XTick'))  ) ; 
title([ 'Channel 1 '  lead1  Afilemits xu int2str(Ra(f))  ' Beat '  int2str(f)] ...
 ,'color', 'r' ,  'FontSize' ,16);  zlen2= [ int2str(f-1) Ca(f-1)  int2str(Ra(f-1)) ];
 TE2 = [ int2str(f+1) Ca(f+1) int2str(Ra(f +1) )] ; 
if Q(f) , text( Q(f ) , xts(Q(f )) ,[ 'Q'   ] , 'color', 'b' , 'FontSize', 14 ); end; 
  text( Ra(f-1) ,xts(Ra( f-1 )), [ zlen2 ] ,'color','r'  , 'FontSize', 14  );
text(Ra(f),xts(Ra(f)), [ int2str(f) Ca(f)  int2str(Ra(f )) ],'color','b', 'FontSize',14);
 text(Ra(f+1),xts(Ra(f +1)), [TE2] , 'color','r' , 'FontSize', 14  );
A1791= [int2str(f+2) Ca(f+2) int2str(Ra(f+2))];
A1= [ int2str(f-2) CA(f-2)  int2str(Ra(f-2 )) ]; zlensort1820I =   [ 'Pe'  ]
if 0, text(Ra(f+2),xts(Ra(f+2)), [A1791], 'color','r' );
    text( Ra(f-2),xts(Ra(f-2)), [ A1]  ,'color','r');
end; fprintf('4020 f%dCa%cPb2%dPe2f%dTb%dTef1%dPb2%dPe%d\n', ...
f,  Ca(f),  Pb2(f ), Pe2f(f ),Tb1f( f ) , Tef1( f ), Pb2(f+1), Pe2f(f+1) );
if ((K2-1)>0), text(Ra(f-1),xts(Ra(f-1)), [int2str(f-1) Ca(f-1) int2str(Ra(f-1))]...
 ,'color','r','FontSize',14);
if  ( Pb2(f-1)>=U), text( Pb2(f-1), xts(Pb2( f-1)), [ 'Pb' ] , 'color', 'r' , 'FontSize',14);
end; if (Pe2f(f-1)>0),text(Pe2f(f-1), xts(Pe2f(f-1)),['Pe'] , 'color','r' ,'FontSize',14);
end; if  (Tb1f( K2-1)> 0),  time=  [ int2str(f-1)  ]; 
 text(Tb1f( K2-1), xts(Tb1f(K2-1) ),[  'Tb1f' ] , 'color', 'r', 'FontSize', 14  ); 
end; if (Tef1(K2-1)>0), 
text(Tef1( K2-1) , xts( Tef1(K2-1)) ,[ 'Tef1' ] ,'color', 'r' , 'FontSize', 14 );
end;end; if (Pb2(f)>0), text(Pb2(f), xts(Pb2(f)), [ 'Pb' ] , 'color', 'b' , 'FontSize',14 ); 
end; if (Pe2f(f)>0),text(Pe2f(f ) , xts( Pe2f(f) ),[ 'Pe'] , 'color', 'b' , 'FontSize', 14); 
end;    text(S( f ) , xts( S( f ) ),[ 'S'   ] ,  'color', 'b'  , 'FontSize', 14 ); 
if 0 &&DBUG, text( Tp(f ) ,  xts( Tp(f ) ),'Tp' , 'color', 'b'  );   
  text( RO(IR-2), xts(RO(IR-2))-0.2  , [ int2str(IR-2) C(IR-2)], 'color','r'   );  
 text( RO(IR-1), xts(RO(IR-1))-0.2   , [ int2str(IR-1) C( IR-1) ], 'color','b'   );  
 text(RO(IR+1), xts(RO(IR+1 ))-0.2, [ int2str(IR+1) C(IR+1)], 'color','b'  ); 
text( RO(IR+2), xts(RO(IR+2))-0.2 , [ int2str(IR+2) C(IR+2) ], 'color','b'   ); 
end;  if Tb1f(f),  text(Tb1f(f ),xts(Tb1f(f  )), [ 'Tb' ], 'color',cb  , 'FontSize', 14);  
end; if Tef1(f),  text(Tef1(f),xts(Tef1(f )), [ 'Tef1' ], 'color',  cb , 'FontSize', 14); 
end; % [ slmaxc{f} , slmaxic{f}]= max(slopes (Limit ,Pb2(f+1)- 1, xts) );
  hold on;   j =1;   te = Tef1( f );   tb = Tb1f( f );
if  ((Pe2f(f)-Pb2(f))<=0), fprintf([' 4060  f=%dCA=%cPb2=%dPe2f=%d'  ...
 'Tb1f= %d Tef1=%d  Pb2=%d Pe2f=%d\n'], ...
  f,  CA(f) , Pb2(f ), Pe2f(f ),Tb1f( f ) , Tef1( f ) , Pb2(f+1), Pe2f(f+1) );       
end; if ( f +1) <= length(Ra), if   Pe2f(f+1),
    text( Pe2f(f+1), xts( Pe2f( f+1 )),[   'Pe' ]   ,'color','r' , 'FontSize', 14 );  
end; if Pb2(f+1), text( Pb2(f+1) ,xts(Pb2(f+1)),['Pb'] ,'color','r','FontSize',14);end;
A1=[ int2str(f+1) Ca(f+1) int2str(Ra(f +1)) ]; 
 text(Ra(f+1),xts(Ra(f+1)), [A1],'color','r', 'FontSize', 14);
end;  end;end; h_940reps(J)=figure(h_940re(J)); 
LE=[d(1:2),d(4:6),d(10:11),d(13:14),d(16:17)];FIGFILE=['ZRepro' Afilemits B];
 FIGUFILE=fullfile(PATFIG, FIGFILE) ;  if DBUG,   dbstop 4100; 
saveas( h_940re(J) , [FIGUFILE 'b' int2str( f ) 'r'  int2str(Ra( f )) LE], 'jpg' )
end; end;  if 1,  dirhea =num2str(s3x) ; dirann =num2str(s4x); 
ETHRE= 0.65 ; RRINT = AX( : , 39 ) ;  [  Pb2Rmax  A11NUM2CME]  
ETHRE =  0.903  ; ED = AX( :, 3) ; uu= unique( ED')
ED0=find(ED==4 | ED == 7 | ED == 8 | ED == 9); RRINTED0=RRINT(ED0); 
ED1 = find ( RRINT >ETHRE  &  ...
( ED  == 4  | ED   == 7  | ED   == 8  | ED   == 9   ...
| ED  == 1 | ED   == 2  | ED   == 3  |   ED   == 11  | ED   == 34)) ;
  ED3 = find (RRINT <= ETHRE & ( ED  ==1| ED == 8)) ; % NUM3 =[ ];
  NUM33 =[ ] ;clear   in RW QW  PbE; 
  Pb2Rmaxst=num2str(Pb2Rmax); a11num2cme=num2str(A11NUM2CME); 

  FDB=[ ACFILE1  '_'  BR d(10:11),d(4:6),  d(1:2),d(13:14),d(16:17)  ...
 BCM BNAN '_'  scof1 '_'  scof2 B  dirann AV 'R' a11num2cme Pb2Rmaxst ];  

FILEAQ=[ 'aq' Afilemits FDB  '.mat']; FILEAQRS= fullfile(PATF,FILEAQ) 
AXQRSV= genvarname(['AXQRS', Afilemits]); eval([AXQRSV '= AXQRS;']);   

FILEAX=[ 'ax' Afilemits  FDB   '.mat']; FILEAXT= fullfile(PATF,FILEAX) 
 AXV= genvarname(['AX', Afilemits]); eval([ AXV '= AX;']);       
 FILEAP=[ 'ap' Afilemits   FDB  '.mat'];
 AXQRSTFV=genvarname(['AXQRSTF',Afilemits]); 
 eval([AXQRSTFV '= AXQRSTF;']); FILEAPT = fullfile(PATF, FILEAP ) 

 FILEAM=['am' Afilemits FDB  '.mat'];FILEAMT= fullfile(PATF, FILEAM) 
AXQRSYMV=genvarname(['AXQRSYM',Afilemits]) ; 
eval([AXQRSYMV  '= AXQRSYM;']);

[ r colam] = size(AX); colu= [1:colam];  
amnan=isnan(AX); Anansum =sum(amnan); [na  anancol]= find(Anansum) 
  if  ~isempty( anancol )  error(' 4092 max value');end; 
Adata1= AX;  Adata2=AXQRS;  Adata3=AXQRSTF; %Adata4=AXQRSYM; 
   clear  AXQRS AXQRSTF AXQRSYM FF*; clear DURA*  AXQRSTF;
end;    if  ~isempty( zn0 ) || 0, Ob-mzo+1  
% return; else   %~exist('Adrive', 'var'),error( '3850 error') ; 
end;  if 0,  F1 = genvarname(['F1', Afilemits]);  
 FILEDAT=[ 'hdata' Afilemits '.mat'];  FILEDAT = fullfile(Pmit, FILEDAT); 
  eval([F1 '= F;']); save( FILEDAT,F1);  
  Hsig = genvarname(['HsigN', Afilemits]); eval([Hsig '= HsigN;']); 
FILEsig=[ 'hsignal' Afilemits d(1:2),d(4:6),d(13:14),d(16:17) '.mat'];  
   FILEsig = fullfile(Pmit, FILEsig); save([ FILEsig ], Hsig );
   AX1=load(FEATUREFILE );uH= unique( HsigN.class)
AM1=eval(['AX1.AX' Afilemits ]); AM=[ AM1(:,1:end) ; ];   
end; if  1, format  long  g; clear(['AM' Afilemits ]);
clear  AVA2   AVAR AUX1; if ~exist('wmethod','var'),wmethod='';end;   
clear('t','TIME');    clear A1F* A1T*  ;   clear  z  yh  F3 F4 A1ST;  
clear  Q1  Q2 QQ  TBE   CA1  TE2v ;  clear   zze* tble1tblen Beatqrs; 
 clear xfn s1valmin s2valmin  F;     clear  GP1 GP2  tt Tb TIME GRL;
 clear  annAmp  banot  fy2  iqrs   xlf  xlf2  X  xd1 xd2 Xpa Xpb X  Te2rr ;   
  clear  z  yh  F3 F4 A1STa  RaFF Te1; clear A1F* A1T*  h F3 F4   ;
 clear ['HsigN' Afilemits] ; clear (['HsigN' Afilemits ]);
 clear  Hsig HsigN pminrpl1  tbls1  ritmin ritmax  ; UCL=length(uH)  
for i = 1 :  colam  [zlAM{i}  zlAMV{i} ]= find (AX( :, i) <=0);end;
end; if 1,  RFF=AX(:,1);  [Aminv  Amini  ]=  min(AX(: , 4:end) ) ;
 [Amax25v  Amax25i  ]=max(AX( : , 4:end) ) ;
Rtestmin =  fix(AX( Amini  , 1)) ;  Rtestmax =  fix(AX( Amax25i , 1)) ;  
 for i =  4 : colam    [MINAMM{i}   MINAMMI{i}] = (min(AX( : , i) )) ;
  [maxamm{i}   maxammI{i} ]  = ( max (AX( : , i) ) );
  MinAMMR{i} =   (  (AX(  MINAMMI{i} , 1) ) ); 
fprintf('4130{%d}%s MIN{i} =%.2f MINI =%dMin=%dMXI%dMX{i}=%.2f\n', ...
i,FEATM{i},MINAMM{i},MINAMMI{i},MinAMMR{i},maxammI{i},maxamm{i});       
end;end; if  10,  % AX=eval(['AX' Afilemits ]);AX=eval([AXV   ]); 
  [ ALzA coluE] = size(AX);  zB= AX;  
  for i =   4 :  coluE
MeanzA{i} =   ( mean (AX( : , i) ) );  %zB( : , i) = zA( : , i)/MeanzA{ i };
  [MINzB{i} MINzBI{i}] =  ( min (AX( : , i) ) );
  [ MAXzB{i}   MAXzBI{i} ]  = ( max (AX( : , i) ) );
fprintf('h 4165  f{%d} %s Mean = %.2f MIN =%.2f MAX =%.2f\n', ...
  i, FEATM{i},  MeanzA{i},   MINzB{i}, MAXzB{i}   );
end; for j =  27 : 34   % 19 :26 (  8 features  27 to 34  )
 if  10, zB( : , j ) = AX( : , j - 8 ) / MeanzA{j - 8};    
 else  zB( : , 24+j ) = AX( : , j - 8 ) ;    
end;  [MINzB{j} MINzBI{j}] =  ( min (zB( : , j) ) );
  [ MAXzB{j}   MAXzBI{j} ]  =   ( max (zB( : , j) ) );
  fprintf('4170  p{%d} %s   Mean = %.2f  MIN =%.2f MAX =%.2f\n', ...
  j , FEATM{j },  MeanzA{j-8 },   MINzB{j }, MAXzB{j } );
 end; end; if  Afilemit~= 214 && Afilemit~= 232  , if  1, 
 wmethodID =1; sup=1;wtr=0; FEATt47=FEATM(4: end); 
  if ~wtr&&sup ,   wtrs = [  'tt'  'sup' Afilemits ] ;   wtrs = [  'tt'  'sup'  ] ; 
elseif ~wtr&&~sup , wtrs = [  'tt' 'uns' Afilemits ] ;  wtrs = [  'tt' 'uns'   ] ; 
end;  if ~exist('zB', 'var'), zB= AX; end; if DBUG, dbstop 4180; end;  
if ~exist('wmethod','var'),wmethod='';end; zBrow1=  zB(1, 1: 4 )
  [ wmethod  wRwF ftScore] = ...
FSLib_v4j16(zB(:, 4 :end),ca, AX(:,4 :end),ca,wmethodID, sup);
if 0, [ wRwFR ftScoreR  wmethod  wRwFt  ftScoret ] = FSLib_v4j16R ...
(TEST4R,TEST4CRM,TRAIN4R,TRAIN4CRM, wmethodID, sup, wtr,alpha);
  if (size(wRwFR,2)~=1),wRwFR=wRwFR'; end;
if  wtr,  wRwF = wRwFR ; ftScore = ftScoreR ;
elseif  ~wtr, if  ~isempty(wRwFt) ,  wRwF = wRwFt ; ftScore = ftScoret;  
else wRwF = wRwFR ; ftScore = ftScoreR ;
end; end;FEATt47rankt= FEATt47( wRwFt );
FEATt47rankR= FEATt47( wRwFR); wLcellMR= length( FEATt47rankR) ;
end; if ~iscell(ftScore)  [ QQM  i]=size(ftScore );
else  i=1; zzzFeaIndexC=ftScore{1, i}';
 end;   
end; if 1, [ fsort   wFwR ] = sort( wRwF); 'findr\' 
   if (size(wRwF,2)~=1),wRwF=wRwF';end;  [ wFea Index]=max( wRwF)   
   FEATt47rank = FEATt47( wRwF );  wLcellMA= length( FEATt47rank) ;  
  for k = 1: wLcellMA 
 len2(k)=length(FEATt47rank{k}); FQPrscelltr{k ,:} =FEATt47rank{1, k };
 for m = 1: len2(k)    NUM3(k , m ) = FQPrscelltr{k}(m);end ;
NUM33=[ NUM33 ' ' NUM3( k ,:) ];    
 [ST3adc1{ k,1}, cTBE2] = sprintf('%s\n', NUM3(k ,:) ); 
 if 0, fprintf('y 4196%d %s\n',k, NUM3(k ,:)); end;
 end;    
 if exist('wRwFR', 'var'),   for k = 1: wLcellMR 
len2R(k)=length(FEATt47rankR{k});
 FQPrscelltrR{k,:}=FEATt47rankR{1,k };
for m = 1: len2R(k)     NUM3R(k,m) = FQPrscelltrR{k}(m);end ;
   [ST3adc3{ k,1}, cTBE2]=sprintf('%-10s', NUM3R(k ,:));
end;[ zX1, strhA1] = xlswrite2( A1FileNxls ,ST3adc3 , sheet+4, AFEATrange );
[ w5,HC1]=xlswrite2(A1FileNXLs,wRwFR, sheet+4,['A' xlcol '3:A' xlcol '51']); 
 end;if exist('wRwF', 'var'),  if (size(wRwF,2)~=1),wRwF=wRwF'; end;
A1FileNxls= [ PATXL  'wfeatureecg'   wmethod wtrs dates  '.xls' ] ;
try
Excel=actxserver('Excel.Application'); 
if  ~exist( A1FileNxls ,'file'), ExcelWorkbook = Excel.workbooks.Add ;
ExcelWorkbook.SaveAs(A1FileNxls,1) ;  ExcelWorkbook.Close(false);   
end;
invoke(Excel.Workbooks,'Open', A1FileNxls) ;
 worksheets = Excel.Sheets ;  
 numSheets = worksheets.Count 
invoke(Excel.ActiveWorkbook,'Save'); Excel.Quit; Excel.delete; clear Excel;
 [ zX, strh] =xlswrite2( A1FileNxls , Afilemit , sheet ,[ 'A' xlcol  '1']) ;     
if wLcellMA&exist(  'ST3adc1' ,'var')%&  ~isempty(ST3adc1)
 [ zX, strhA] =xlswrite2( A1FileNxls, ST3adc1, sheet ,AFEATrange);
[w5c,HC] =xlswrite2(A1FileNxls,  wRwF,sheet, ['A' xlcol '3:A' xlcol '51']);
  [ zX , strh] = xlswrite2( A1FileNxls , Afilemit , sheet  , AfileC1 ) ;    
 end; 
 
 [ w5c, HC1 ] = xlswrite2( A1FileNxls,  wRwF, sheet , AFEATrange) ; 
 [wsts2, wms] = xlswrite2(A1FileNxls, Afilemit, sheet, AfileC1) ; 

 [ zX , sth] = xlswrite2( A1FileNxls , Afilemit , sheet+4 , AfileC1 ) ;    

[ zcX ,strh] = xlswrite2( A1FileNxls , Afilemit , sheet+4, [ 'A' xlcol  '1'] ) ;     
catch exception
    fprintf('4200Error: %s:%s\n',exception.identifier,exception.message);
end;end; 
end;end;   if 0,  j=0;  aberrr= []; ab_err = []; 
  for col1 =  4 : 25    j = j+1 ;  % j= col1; %  for col1=1: colam         
 FC(: , col1)  = AX( :, col1 ) ; AMmin(col1) =  min(FC(: , col1) ) ; 
[AMVmax(j) AMmaxI(j)] =max(FC(: ,col1)); AMR(j) =AX(single(AMmaxI(j)),1); 
 AMR2{ j}=  int2str(AMR(j) ) ;  slen= length(AMR2{j}) ;  
 if (slen~= 6), RTEST = AMR2{j}(1:slen); else RTEST =AMR2{j}(1:slen); 
end;  RTESTS=str2num(RTEST) ; 
if isempty(RTEST) || isempty(RTESTS),RTEST= AMR2{j}(1:slen) ;
 RTESTS = AMR2{j}(1:slen) ;    
 end;   RTESTS= str2num(RTEST) ; 
[ RTESTI(j) ] = find( ( Ra == (RTEST) ) |   ...
  (Ra== RTESTS-1)| (Ra == RTESTS+1)| (Ra ==RTESTS-2) ...
| (Ra==RTESTS+2) | ( Ra == RTESTS-3)  ) ; A2(j)= RTESTI(j)  ;
if ( Pb2( RTESTI(j) )== 0 )&& (Pe2f( RTESTI(j) ) ~= 0 ), 
  error(' 4142 max value');
end;    [zlNFC{j}  zlNFCv{j} ] = find(FC( :, col1 ) <=0); 
COL = AX( :, col1 ) ; [MinC In  ]  = min( COL) ;  minR = AX(In , 1); 
aberrr = [ aberrr minR]; ab_err = [ ab_err In];  [MaxC  Ix]= max( COL) ;
end;    AB2= sort(A2) ;  
 fidM = fopen('200.txt', 'at+');   fprintf(fidM , '%d ', AMR ) ;
fseek(fidM , 0 , 'bof'); AB3= fscanf(fidM , '%10d %10d', [1  inf]); 
  AB4=fscanf(fidM , '%10d', inf )  ;  
for i = 1 : colam      [zzFAMmin(i)  zzFAMmins3(i) ] =min( AX(: , i ) ) ; 
   [zzNFC{i}  zTwsRC{i} ] = ( find( AX(:,i) == zzFAMmin(i) ) );  
  end;  CAM =AX(:, 3);    CNormam =find (CAM== 1);
  absort=sort( aberrr ); ABUNIQR=unique( absort);absort1= sort(ab_err);
  ABUNIQ1=unique( absort1);[rnfc cfc] = size( zzNFC{i}'  ) ; 
  if  ~isempty(zTws) , zTws = zTws(1:  length( zTws)-1 )
zTwsR = AX( zTws ,1) ;  zTwsC = AX(zTws, 3);  uztwc=unique(zTwsC) ;
 for i = 1: length( uztwc )    ztwsc{i} =    find( zTwsC == uztwc(i)) ; end;
 zTwsRC = { AX(zTws , 1)} ; RTws= zTwsRC{1,1} ;
twsRstr=num2str(zTwsR(1:end)); twsr= twsRstr(: ,4:end);ztwsnum=str2num(twsr);
  end; end; if  1, ALL= Afilemit ;  clear pbls1ar;
 fprintf(' 4235  Complete  \n');   clear zlAM*  cnpcn;  
 clear  Cnorm  c    xlf2   TPR CL  ROCNPT  TrangeTE Annottime Asubtyp ;
clear zlen2  zlen2   zlen1170  zpp  CRBB AXPQRST antyp ; %y= +2269
clear   pb*  VAL* NUMFIELD SUBTYPEFIELD RW TEf QT* zB2;
clear A1 annot annt  banot   ANNT ANNOT AMP*  SUBTYPEFIELD  POS*;
clear    POS_ANNOT QW  RA* RR*  Aaux Pe2fM  flag  ANNTR     ;
clear    Rprev  SW TE  T     RS*  ro* Tp TeS*   Der DM zA2;
clearvars  AMM AXPQRST AXQRS AXQRSTF   FC  HsigN   Block* ;
clearvars S2  SO Tef1O  tbl* Pb1  PeS  PeakT amnan  cA* FM   zPW* ;
clearvars D E F1 xr T FFRSXA   -except  FFBLPOST NFFRRPREV  ;  
clearvars  TePb Pb2R Pb2Te  STb AMM  AXQRS  RPSTP  BL* FC ;
clear annAmp RRR  AX  AXQRSYM    O aux2 aul  AQRS    ; 
clearvars   ANNRO    CNormam     xd1    HsigN ;
clear RO32 ROCP STe STeI    AVG   RFF  RO1 TPM STb  ;
clear  ANNtime CLL C_AB   cl cla  uRO uROI  zsNUM0  CF PeRange2 ; 
clear A67 Rac RacI ca4 ca3 ca2 Rpvc Cbbl Racl ; clear ps ps2 ;  
 clear  zlen*   ia ib AIR1180*   zp zrow TE2*  Cn*  FEAT* FI Fusion FF* ; 
clear  Tef2 Tef3  Tb1fO TBE2  SM2  Rp2 QM QQM; clear   Rit*; 
clear xts; cd 'C:\FINAL\dropbox' ; %&& 1>length( zn0 ),return;end;
if DBUG, dbstop 4285; end;   [  Pb2Rmax  A11NUM2CME] 
if  isequal( B,'BLN' ),  BLIN = 'BLIN'; end; zzlen
if 1||  (  isequal( B,'BL') && LAGUNA && Bthesis) , 
save( FILEAPT, AXQRSTFV ); save( FILEAMT , AXQRSYMV); 
save( FILEAXT , AXV ); save( FILEAQRS , AXQRSV ) 
 end; end; end; if 1,if  0&& isequal(BO,'TT'),jagakm15_170708; end;
if exist('FPIR','var'),  if length(FPIR)>1, svmr= 1; R= sort(FPIR) ;
 screen = get( 0, 'ScreenSize' ) ;   minxbl1= min( xbl1( U : V ))-0.5  ;
else    svmr = length(FPIR) ; R= sort(FPIR) ;% jensem150828;
end; for i  =  1  :  svmr      k = i ; % jens150828; 
    IR =  find( RO == R(i)    | RO ==(R(i)+1 )  |    RO ==(R(i)-1 ) ) ;
    J= find( Ra == R(i)    |    Ra ==(R(i)+1 )  |    Ra ==(R(i)-1 )  ) ;
    f =  J  ;  CLA=Ca(f) ;  RI = Ra(f) ; K2= f;%(Afilemit , Afilemit , Adata2) ; 
if exist('xbl1','var') || exist('xbl1','var') || exist('xts1m' ,'var') || exist('xts', 'var')
hFP= figure('NAME', [Afilemits '4300 FP ' num2str(f) num2str(RI)] ,'Position',...
  [screen(3)/2 40  screen(3)/2-10 screen(4)/2-60]);
U=Ra(f-1)-100; V=Ra(f +1)+100; if  (U<=0) U=10; end; if (V>=NS) V=NS-100;end;
   subplot(2,1,1) ;    grid off;     hold on; %pending replace xbl1 by xts 
if  exist('xbl1', 'var'), plot(U : V , xts( U : V) , 'r' , 'LineWidth', 2 );  
 title( ['4210 FP  Test ' int2str(f)  ' ' Afilemits ],'color', 'r' , 'FontSize' ,16);
   hold on;   axis([U V   min( xts( U : V))  max( xts( U : V) )  ]);  
 if ( Tb1f( K2-1)),
 text( Tb1f( K2-1), xts(Tb1f( K2-1)),[ int2str(f-1) 'Tb1f' ],'color', 'r' ,'FontSize', 14) ;
  end; if (Tef1(K2-1)> 0),  
 text(Tef1(K2-1),xbl1( Tef1(K2-1)),[ int2str(f-1) 'Tef1' ] ,'color', 'r' ,'FontSize',14); 
 end; if  ( Tb1f( K2) > 0 )  text(Tb1f(K2), xbl1(Tb1f(K2)),...
[int2str(f) 'Tb' int2str(Tb1f(K2))], 'color','b','FontSize',14);  
  end; if    exist( 'Tef1' , 'var'), if  ( Tef1( K2) > 0 )
text(Tef1(K2), xbl1(Tef1(K2)),[int2str(f) 'Te' int2str(Tef1(K2))] ,'color','r','FontSize',14); 
end; end; if  (Pb2(f) >0),
    text( Pb2( f) , xbl1(Pb2( f)),[ int2str(f)  'Pb2' ] , 'color', 'b', 'FontSize',14 );
end;  if ( Pe2f(f)>0),  
 text(Pe2f(f), xbl1(Pe2f( f)),[ int2str(f) 'Pe' int2str(Pe2f(f)) ] ,'color', 'b','FontSize',14);  
end; text(Ra(f-2), xbl1(Ra(f-2)),...
    [ int2str(f-2) Ca(f-2) int2str(Ra(f-2))],'color','r', 'FontSize',14  );
text(Ra(f-1),xbl1(Ra(f-1)), [ int2str(f-1) Ca(f-1) int2str(Ra(f-1))],'color','r', 'FontSize',14);
text( Ra(f), xbl1( Ra(f)) , [int2str(f)  Ca(f) int2str(Ra(f )) ],'color','b', 'FontSize',14);
text(Ra(f+1),xbl1(Ra(f+1)),[int2str(f+1) Ca(f+1) int2str(Ra(f+1))],'color','r','FontSize',14);
 if Pe2f(f+1), text(Pe2f(f+1),xbl1(Pe2f(f+1 )),  'Pe' , 'FontSize', 14 );  end ;
  if Pb2(f+1), text(Pb2(f+1 ),xbl1(Pb2(f+1 )),  'Pb' , 'FontSize', 14 );  end ;
if Q(f), text(Q(f),xbl1(Q(f )), 'Q', 'FontSize',14); end; %text(Tp(f) ,xts(Tp(f) ),'Tp');
fprintf('4300 f=%d Ra= %d R=%dRa(i)=%f \n', f, Ra(f), RO( IR ), xbl1(Ra(f)));
%fprintf(' 471 K2= %d  i=%d Pb2= %d Pe2f=%d Q=%d Ra=%d S=%d \n',...
%    K2, i,Pb2(K2),Pe2f(K2) ,  Q(K2),Ra(K2) ,S(K2) );  
 text( RO(IR-1) ,xbl1(RO(IR-1)), [ int2str( IR-1) C(IR-1) int2str(RO(IR-1)) ],'color','r');
end; if exist('xbl1' , 'var'),  subplot(2,1,2) ;   grid off;
 if  exist('xts1m', 'var'), plot(U : V , xts1m( U : V) , 'r'  , 'LineWidth', 2 ); hold on; 
end;  plot(U : V, xbl1( U: V) , 'b' , 'LineWidth', 2); hold on;
 plot(U : V , xbl1( U : V) , 'r' , 'LineWidth', 2);  hold on;  
 axis([U V   min( xbl1( U : V))  max( xbl1( U : V) )  ]);  
text(Ra(f-1), xbl1(Ra(f-1)),[ int2str(f-1) Ca(f-1) int2str(Ra(f-1))],'color','r', 'FontSize',14);
if 0, text(Rac(f), xbl1( Rac(f)), [int2str(f)  Ca(f) int2str(Rac(f))] ,'color', 'r','FontSize',14);
end; text( Ra(f) , xbl1(Ra(f)) , [int2str(f) Ca(f) int2str(Ra( f))] ,'color','b', 'FontSize',14);
if exist( 'Tef1','var'), fprintf('4315 Tb1f= %dTef1(K2)= %d \n', Tb1f(K2),Tef1(K2)) ;
 end; end; FIGFILE=[ 'FP' Afilemits d(1:2),d(4:6),d(10:11),d(13:14),d(16:17)];
FIGUFILE = fullfile(PATFIG, FIGFILE) ; if DBUG, 
 saveas( hFP, [FIGUFILE '_' int2str(f) B ], 'jpg')
end; end;  end; end;   if   exist('TPIR' , 'var'),  R= sort(TPIR) ; 
if  length(TPIR) >1  L= 1 ; else L= length(TPIR) ;  end; for i =  1 : L       
% IR =  find( RO == R(i))  ;  % |  RO ==(R(i)+1 ) |  RO ==(R(i)-1 );
 J= find( Ra == R(i) )  ; % |    Ra ==(R(i)+1 )  | Ra ==(R(i)-1 )   ; 
  f =  J  ; CLA=Ca(f) ;  RI = Ra(f) ; K2= f;  if K2==1,  continue; end;    
hTP = figure('NAME', [' 4350  TP TEST ' num2str(f) '_' num2str(RI) ] ,...
  'Position',   [screen(3)/2 40  screen(3)/2-10 screen(4)/2-60]);
   U = Ra( f -1)-100;  V = Ra( f + 1) +100 ;  
   if (U <=0), U=100; end; if  (V >=NS) ,V = NS -100;end;
   subplot(2,1,1) ;    if    exist( 'xts' , 'var'), 
 plot(U : V , xts( U : V) , 'r'  , 'LineWidth', 2 ); grid off;  
   axis([U V   min( xts( U : V))  max( xts( U : V) )  ]);
 end;  title( ['TPTest' Afilemits  'Beat no:' int2str(f) ],'color', 'r', 'FontSize',16); 
 if    exist( 'xts' , 'var'),   if    ( Pb2( f )> 0)
 text( Pb2( f ) , xts( Pb2( f) ),[int2str(f)   'Pb2'  ] , 'color', 'b' );  end;
if ( Pe2f(f)>0),text(Pe2f( f), xts( Pe2f( f) ),[ int2str(f)  'Pe2f'] ,'color', 'r'); end;
     text( Ra(f-1 ) , xts(Ra( f-1 )) , [ int2str(f-1) Ca(f-1) ],'color','r');
     text( Ra(f-1 ) , xts(Ra( f-1 )) , [ int2str(f-1) Ca(f-1)   ],'color','r');
     text( Ra(f ) , xts( Ra( f ) ) , [int2str(f)  Ca(f) int2str(Ra( f ) ) ],'color','b');
     text( Ra(f+1 ) , xts(Ra( f +1)) , [int2str(f+1) Ca(f) ],'color','r'  );
fprintf('4335 f=%dRa=%d%d%d=%d\n', f, Ra(f), RO(f), xts(Ra(f)), xts(RO(f)));
 text( RO(IR ) , fix(xts( RO(IR) ) ) , [int2str(IR)  Ca(f) int2str(RO(IR) ) ] ,'color','b');
     text(  Ra(f ) , xts(Ra(f) ) , [int2str(f)  Ca(f) int2str(RO( IR ) ) ] ,'color','b'  );
     if ( Tb1f( K2)  > 0),
text(Tb1f(K2), xts(Tb1f(K2) ),[int2str(f) 'Tb1f' int2str(Tb1f( K2))] ,'color','b'  ); 
 end;  if   exist( 'Tef1' , 'var'),  if ( Tef1( K2) > 0),
text(Tef1(K2),xts( Tef1( K2) ),[int2str(f) 'Tef1' int2str(Tef1( K2))] ,'color', 'r'  ); 
end; end;    if Pe2f(f+1 ) text(Pe2f(f+1 ),xts(Pe2f(f+1 )),'Pe'    );   end ;
     if Pb2(f+1 ) text(Pb2(f+1 ),xts(Pb2(f+1 )),'Pb'     );    end ;
if Q(f ),  text(Q(f ),xts(Q(f )),'Q'); end; %  text(Tp(f ) ,xbl1(Tp(f ) ),'Tp');   
if exist('S' , 'var'), if (S(f ) > 0) && (~isempty(S)) ,
text(S(f ), xts(S(f )),'S', 'FontSize',16);   end;
end; if exist( 'Rp', 'var') &&(Rp(f ) >0) && (~isempty(Rp)),
text(Rp(f), xts(Rp(f )),  'rp'  , 'FontSize',16  );
end; end;  if   exist('xts' , 'var'), subplot(2,1,2) ;
plot(U : V , xts( U : V) , 'r'  , 'LineWidth', 2 );     hold on;
axis([U V   min( xts( U : V))  max( xts( U : V) )  ]);
if exist('Tef1', 'var') &&(Tef1(f) >0), text( Tef1(f ), xts(Tef1(f )),'Te', 'color','r');  
end; end; if 0, Rg= Ra(f);  j=1;
TeS(f) = Q(f+1)-100;  TeE(f)= TeS(f)-150; te = Tef1( f ) ;  tb = Tb1f( f )  ;
 end;  d=datestr(now) ; 
 FIGFILE=  [ 'TP4486' Afilemits d(1:2),d(4:6),d(10:11),d(13:14),d(16:17) B];
 FIGUFILE = fullfile(PATFIG, FIGFILE); if DBUG, 
  saveas(  hTP , [FIGUFILE '_' int2str(f) B ], 'jpg' )  ;
end; end;  end; if 0,  choose=input(' 4800   1 for medoid, e for exit \n','s');
while ~strcmp(choose,'e')
 switch choose
 case '0'
                fprintf('Press any key');
            case '1'
            load(FILEQRS);    kmedoid(data,param);   %1208
        case '2'    % d=[ da(10:11),da(4:6), da( 1:2)]; 
    end; choose=input('Which demo  you want?  e for exit \n ','s');
    end; end ;   Atimemain1 = toc /60; 
clear  amnan    FF FC   AMM   cnptoxc  Pb1  TE2v ;  clear *range* ; 
  fprintf(' 4365 Elapsed time in minutes %.2f \n', Atimemain1); 
clearvars  TeS* pbls1   ;   clearvars  TeE*; % %wavevaluate4(xts'); 
 clearvars D  E F1 xr   AX AP AQ*     F FF AXQRS  AXPQRST ; 
   Atimemain2 =toc/60 ; clear  AVA cnptoxc ; 
waaaj2=['pefilter_param' 'pethreshparam'  'pbparam','pbslope_thres' 'ptparam']; 
   %%set(gca, 'XTickLabel', num2str(get(gca, 'XTick'))) y = downsample(x,n)
 warning off MATLAB:pack:InvalidInvocationLocation
  end;  if  1,  clear  xd1;  dbclear all ;  Ob
computer('arch') ;  java.lang.Runtime.getRuntime.maxMemory
java.lang.Runtime.getRuntime.totalMemory
java.lang.Runtime.getRuntime.freeMemory  
version -java
dervz =  {  'LagQ' , 'PoffQ ' , 'PeRange2max','pept_thresh', ...
 'PbSPb2max',  'PbSPe2fmax','pb_thresh' , 'SlopemaxI' ,  ...
 'TBs2Smax' , 'wTbSmax' , 'slope_threshTb',  'pt_threshTb',...
 'TeS3Tef1max' , 'Te3range3max' , 'pt_threshTe' , 'Te3first_maxmax' ,  ...
 'Pw' , 'Pw' , 'Tw',  'Tw', '0', 'NUM2' , 'MIN' , 'MAT' ,  'toc' ,  ...
  'pefilter_param', 'pethresh_param','pb_param' , 'pbslope_thresh', 'pt_param',...
   'Pb2Rmean', 'Pb2Rs ', ' RTemean ', 'RTes ', ' Pb2Temean', ' Pb2Tes',  ...
   'Afilemit',  'Pb2Rmax', 'RTemax',  'A11NUM2CME', '0',  ...  
   };    % set( gca,  'XTickLabel', (get(gca, 'XTick'))) ;
 
 end;
%207 440 flutter
%106 410 rhythm change
%101 660 noise 
% %222 b675 1j, 
%208 1n b3, 2s b1774, 3v b2
%223 4b191,  2Ab86, 
