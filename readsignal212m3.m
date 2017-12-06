  function [ hp , ATRTIMED, Annot, M, h ]= readsignal212m3( recname,ns,t1,t2, PATFIG  );    
%     dbstop 75 ;    dbstop 81;  % %recname='d:\data\mitdb\118';t1=1;t2=1000;
%PATH= 'd:\data\mitdb'; % path, where data are saved %global
if ~exist('recname','var')     close all ; clear all ;  clc;  
    recname='c:\data\mitdb\featurenew\105'; 
end;SCR = get(0,'ScreenSize');  %dbstop 12 ; 
% if ~exist('t1recname','var') recname='d:\data\mitdb\200'; end;
  DATAFILE=[recname '.dat']; signalh=[recname '.hea']; 
  rname = recname(length(recname)-2:end);    % HEADER   
fprintf(1,' $> WORKING ON HEA  %s ...\n', signalh); 
fid1=fopen(signalh,'r');% 3 
z= fgetl(fid1);  
A= sscanf(z, '%*s %d %d %d',[1,3]);   nosig= A(1);  % number of signals
sfreq=A(2);  nsamp=A(3); %ns=header.nsamp;
if ~exist('t2','var') t2=2000; end;   if ~exist('t1','var') t1=1; end; 
for k= 1 :  nosig         % ML2 V1
    z = fgetl(fid1);       %char(B(8:11))
    A1= sscanf(z, '%*s %d %d %d %d %d',[1,5]);    dformat(k)= A1(1);% format; only 212 is allowed
    gain(k)= A1(2);              % number of integers per mV
    bitres(k)= A1(3);            % bitresolution
    zerovalue(k)= A1(4);         % integer value of ECG zero point
    firstvalue(k)= A1(5);        % first integer value of signal (to test for errors)
    B= sscanf(z, '%*s %d %d %d %d %d %d %d %s ',[1 8]); 
    lead(k,1:length(B)-7)=char(B(8:length(B)));
end;  signald= DATAFILE; DN =3*360 ; start = 1 ;% fclose(fid1);  
 %------ DATA    % data in format 212
fid2=fopen(signald,'r');   fprintf(1,'\\n$> WORKING ON DATA   %s ...\n', signald);
A1= fread( fid2, [3, t2], 'uint8')';  % matrix with 3 rows, each 8 bits long, = 2*12bit
fclose(fid2);  M2H= bitshift(A1(:,2), -4);
M1H= bitand(A1(:,2), 15);  PRL=bitshift(bitand(A1(:,2),8),9);     % sign-bit
PRR=bitshift(bitand(A1(:,2),128),5);   % sign-bit
M( : , 1)= bitshift(M1H,8)+ A1(:,1)-PRL; M( : , 2)= bitshift(M2H,8)+ A1(:,3)-PRR;
if M(1,:) ~= firstvalue, error('inconsistency in the first bit values'); end;
h = M(start:DN ,:) ;  
hp( : , 1)= (h( : , 1)- zerovalue(1)); hp( : , 2)= (h( : , 2)- zerovalue(2)) ;  
switch nosig
case 2
    E1=M(1,1);       E2= ( M(1,1)- zerovalue(1) ); E3=E2/gain(1);
    M( : , 1) = (M( : , 1)- zerovalue(1)  ) /gain(1);
    M( : , 2) = (M( : , 2)- zerovalue(2))/gain(2) ;   % TIME=(t1:(t2-1))/sfreq;
    TIME=(t1:(t2))/sfreq;
case 1
    M( : , 1)= (M( : , 1)- zerovalue(1));       M( : , 2)= (M( : , 2)- zerovalue(1));
    M=M';       M(1)=[];    sM=size(M);        sM=sM(2)+1;
    M(sM)=0;        M=M';        M=M/gain(1);      TIME=(t1:2*(t2)-1)/sfreq;
 
otherwise  % this case did not appear up to now!    % here M has to be sorted!!!
disp('Sorting algorithm for more than 2 signals not programmed yet!');
end; atrd=[recname '.atr'];  clear A M1H M2H PRR PRL; 
%fprintf(1,'\\n$> LOADING DATA FINISHED \n'); %LOAD ATTRIBUTES DATA -
fprintf(1,'\\n$> WORKING ON ATR ANNOT %s ...\n', atrd);  % attribute file with annotation data
fid3=fopen(atrd,'r');    A= fread(fid3, [2, inf], 'uint8')';
fclose(fid3);   ATRTIME=[];  ANNOT=[];  sa=size(A);  saa=sa(1);  i=1;
while i<=saa
    annoth=bitshift(A(i,2),-2);
    if annoth==59,        ANNOT=[ANNOT;bitshift(A(i+3,2),-2)];  
        ATRTIME =[ATRTIME;A(i+2,1)+bitshift(A(i+2,2),8)+...
                bitshift(A(i+1,1),16)+bitshift(A(i+1,2),24)];        i=i+3;
    elseif annoth==60,         % nothing to do!
    elseif annoth==61,         % nothing to do!
    elseif annoth==62,         % nothing to do!
    elseif annoth==63,  hilfe=bitshift(bitand(A(i,2),3),8)+A(i,1);  
        hilfe=hilfe+mod(hilfe,2);  i=i+hilfe/2;
    else   ATRTIME=[ATRTIME;bitshift(bitand(A(i,2),3),8)+A(i,1)];
        ANNOT=[ANNOT;bitshift(A(i,2),-2)];
   end;     i=i+1;
end;  ANNOT(length(ANNOT))=[];       % last line = EOF (=0)
ATRTIME(length(ATRTIME))=[];   % last line = EOF
clear A A1;   ATRTIME= (cumsum(ATRTIME))/sfreq;  ATRTIME= ATRTIME *sfreq;
ind= find((ATRTIME <= t2) & (ATRTIME >= t1) )  ;
ATRTIMED= ATRTIME(ind);   ATRTIMED=round(ATRTIMED);
ANNOT=round(ANNOT); ANNOTD= ANNOT(ind);  DN=t1-1+3*360;
DB=find(ATRTIMED>t1 & ATRTIMED<DN); DR=ATRTIMED(DB);
stringlead=['1. ECG lead1#  \color{magenta} '  lead(1,:) ...
    ' \color{red}  data of  MIT-BIH Patient #  \color{magenta} ' rname ];
string1=[ '1. ECG data   of  MIT-BIH Database  Record#  \color{magenta} '  rname    ];
string2=['2. Subtracted by ADC zerovalue used in Digitization: \color{magenta} ' int2str(zerovalue(1))];
string3=['3. Deamplified by ADC gain'    ' used in Digitization: \color{magenta} ' int2str( gain(1)) ];
if   ~exist('ns','var')  || 1 ,file1= mfilename('fullpath')
    hupper = figure('Name', [ mfilename('fullpath')  '76ECG'  (rname)],...
 'Position',   [ 0 0  SCR(3)/1-50 SCR(4)/1-50]);h1=gcf; set(h1,'WindowStyle','docked'); 
    set(gca  ,'FontSize',18  );      hsub1 = subplot(3,1,1);   
    min1=   min( h(start:1080,1))  ;      max1= max(h(start:1080,1) )
    set(gca,   'Units', 'normalized',    'XTick' ,  0:1080/3:1081,   'YTick',   min1:(max1-min1)/2:max1,...
        'FontUnits', 'points', 'FontWeight','normal',  'FontSize', 22, 'FontName','Times' ) ;
    plot( start : 1080 , h(start:1080,1), 'b' , 'LineWidth', 2  );  hold on;
% plot( start : 1080 , h(start:1080,2), 'r'   );  hold on;
        hs=  title(hsub1,  [ string1 ] ,'Color','r');   xlim([1  1084]) ; ylim([ min1-10 max1+10  ]) ;
        text(DR ,   h(DR,1),   'R'  );    ylabel('Amplitude ',   'FontSize',16     ); 
        text(DR ,   h(DR,1),  int2str( DR)  )  ;   
          set(gca,   'YTick',   min1:(max1-min1)/2:max1 ) ; 
        set(gca,'XTick', 0:1080/3:1086) ;  set(gca,'XTickLabel',   { 0:1080/3:1086}   )  ;      
     
    subplot( 312);   box on, hold on;   min2=   min(hp(start:1080,1)) ; max2= max(hp(start:1080,1) ) ;
        set(gca,  'Units', 'normalized', 'FontUnits', 'points',  ...
 'YTick', min2:(max2-min2)/2:max2  ,  'FontWeight','normal',  'FontSize', 22, 'FontName','Times' ) ;
plot( start: 1080,  hp(start:1080,1), 'b'  , 'LineWidth', 2) ; 
%   plot(TIM ,M(t1:t1-1+3*360,2), 'r'); hold on;
        title([ string2] ,'Color','r');         xlim([0  1084]) ;   ylim([ min2-10 max2+10  ]) ;
        set(gca,'XTick', 360:1080/3:1086  ) ;  set(gca,'XTickLabel',   { 1:1 :3}   )  ;     %
        text(DR,   hp(DR,1),   'R'  );  
        xlabel( 'Time in seconds ',   'FontSize',16) ; 
        TIM=(t1:(t1-1+3*360))/1;     box on, hold on
  ylabel(  'Amplitude ',   'FontSize',16     ); 
        subplot( 313);    min3=   min(M(start:DN,1) ) ; max3= max(M(start:DN,1) ) ; 
  set(gca,  'Units', 'normalized',  'FontUnits', 'points',   ...
'YTick', min3:(max3-min3)/2:max3 ,  'FontWeight','normal',  'FontSize', 22, 'FontName','Times' ) ;    
 plot( TIM,  M(t1:t1-1+3*360 ,1), 'b' , 'LineWidth', 2  ) ; hold on;     xlim([0  1084]) ;   
 if 0,    plot(TIM ,  M(t1:t1-1+3*360 ,2), 'r'   )  ; hold on;  
 text(   -15 ,  -1 , [ '\fontsize{16}black {\color{magenta} magenta '...
'\color[rgb]{0 .5 .5}teal \color{red}red} black }' ]) ; 
   end;      Ax=   0.6; Ay=   0.07;   Bx=   0.8;      By=  Ay; 
har = annotation('textarrow',  [Ax  Bx ] ,  [ Ay  By ]  )  ;
set(gca,'XTick', 360:1080/3:1086) ;   ylim([ min3-0.10 max3+0.10  ]) ;
set(gca,'XTickLabel',   { 1:1 :3}   )  ;    text(DR,   M(DR,1),  int2str( DR)  )  ;
set(gca,   'YTick',   min3:(max3-min3)/2:max3 ) ;   title([ string3     ] ,'Color','r');  
  ylabel(' Amplitude (mV) ',   'FontSize', 16     ); 
text(DR,   M(DR,1),   'R'  );   xlabel( 'Time in seconds ',   'FontSize',16) ;
ds=datestr(now) ;   PATFIG ='C:\FIGURE\' ;
%xlim([TIME(1), TIME(end)]);  %xlabel('Time / s'); ylabel('Voltage / mV');
cnptoqc= [ rname 'DeamplifiedbyADCgain'  ] ; %title(string); global ECG; ECG=M(:,2);
FIGFILE= [ cnptoqc ds(1:2), ds(4:6), ds(10:11), ds(13:14), ds(16:17)   ];
FIGUFILE = fullfile(PATFIG, FIGFILE);   % saveas(  hupper , [ FIGUFILE    rname  'png']  , 'png') ; 
%  saveas(  hupper , [ FIGUFILE  rname    'tif'  ]  , 'tif')  ; 
 Annot.time=ATRTIME;  
Annot.anntyp=ANNOT;  % figure(2); % plot(TIME*sfreq,M(t1:t2-1,2),'r'); hold on;
% ATRTIME = ATRTIME+2; %text(TIME,ECG(TIME),'R');
% for k=1:length(ATRTIME)-2 %    text(ATRTIME,ECG(ATRTIME),'R');
%%num2str(ANNOTD(k))); % end;%h=[h1 h2];  %fprintf(1,'\\n$> DISPLAYING DATA FINISHED\n');
warning off MATLAB:declareGlobalBeforeUse;   %global hupper;
close( hupper);  fprintf(1,'\\n$> ALL FINISHED \n');
end; 