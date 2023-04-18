data = csvread('db_stat75.csv',1,1,[1,1,100,2]);

%%%%%%%%%%%
%Partie: 1%
%%%%%%%%%%%

%monde
nat_tot=sum(data(:,1));
mort_tot=sum(data(:,2));

%calcul moyenne
moy_nat=mean(data(:,1));
moy_mort=mean(data(:,2));

%calcul médiane
med_nat=median(data(:,1));
med_mort=median(data(:,2));

%calcul mode
mode_nat=mode(data(:,1));
mode_mort=mode(data(:,2));

%calcul écart-type
ecart_type_nat=std(data(:,1));
ecart_type_mort=std(data(:,2));

%taux normaux
norm_nat = [moy_nat-ecart_type_nat;moy_nat+ecart_type_nat];
norm_mort=[moy_mort-ecart_type_mort;moy_mort+ecart_type_mort];
nombre_nat_norm = length(find(data(:,1)>=norm_nat(1) & data(:,1)<=norm_nat(2)));
nombre_mort_norm = length(find(data(:,2)>=norm_mort(1) & data(:,2)<=norm_mort(2)));

%quartiles
Q1_nat=quantile(data(:,1),0.25);
Q3_nat=quantile(data(:,1),0.75);
Q1_mort=quantile(data(:,2),0.25);
Q3_mort=quantile(data(:,2),0.75);
%valeurs aberrantes
ab_nat=[Q1_nat-(1.5*(Q3_nat-Q1_nat)) Q3_nat+(1.5*(Q3_nat-Q1_nat))];
ab_mort=[Q1_mort-1.5*(Q3_mort-Q1_mort) Q3_mort+1.5*(Q3_mort-Q1_mort)];

%proportion
prop=length(find(data(:,1)<20 & data(:,1)>data(8,1)))/100;


%%%%%%%%%%%
%Partie: 2%
%%%%%%%%%%%

%point a%

%echantillon
r=randsample(1:100,20);
ech20=data(r,:);

moyenne_20=mean(ech20);
mediane_20=median(ech20);
ecart_type_20=std(ech20);

%distance K S
d1=kstest2(data(:,1),ech20(:,1));
d2=kstest2(data(:,2),ech20(:,2));


%point b
%500 echantillons
A=cell(500,1);
M1=zeros(500,2);
M2=zeros(500,2);
M3=zeros(500,2);
M4=zeros(500,2);

for i=1:length(A)
r=randsample(1:100,20);
A{i}=data(r,:);
end

%bi moyenne
for i=1:length(A)
    M1(i,:)=mean(A{i});
end
m1=mean(M1);
moyenne_mean_tn=m1(1);
moyenne_mean_tm=m1(2);

%bii mediane
for i=1:length(A)
    M2(i,:)=median(A{i});
end
m2=mean(M2);
moyenne_median_tn=m2(1);
moyenne_median_tm=m2(2);

%biii ecart-type
for i=1:length(A)
    M3(i,:)=std(A{i});
end
m3=mean(M3);
moyenne_mean_tn=m3(1);
moyenne_mean_tm=m3(2);


%distances de Kolmogorov Smirnov
D=zeros(500,2);
for i=1:500
    D(i,1)=kstest2(data(:,1),A{i}(:,1));
    D(i,2)=kstest2(data(:,2),A{i}(:,2));
end


%%%%%%%%%%%
%Partie: 3%
%%%%%%%%%%%
A=cell(100,1);
b=cell(100,1);
mean_nat=mean(data(:,1));

for i=1:100
r1=randsample(1:100,20);
r2=randsample(1:100,50);
A{i}=data(r1,1);
b{i}=data(r2,1);
end

%3a
%moyenne
mx20=zeros(100,1);
mx50=zeros(100,1);
for i=1:100
    mx20(i)=mean(A{i});
    mx50(i)=mean(b{i});
end


biais_mx20 = mean(mx20 - mean_nat);
var_mx20 = mean((mx20 - mean_nat).^2)-(biais_mx20)^2;
mean_var20 = var(mx20,1);


%3b
%mediane
medianx20=zeros(1,100);
medianx50=zeros(1,100);
for i=1:100
    medianx20(i)=median(A{i});
    medianx50(i)=median(b{i});
end

biais_medianx20 = mean(medianx20 - mean_nat);
var_medianx20 = mean((medianx20-mean_nat).^2) - (biais_medianx20)^2;
median_var20 = var(medianx20,1);


%taille=50
biais_mx50 = mean(mx50 - mean_nat);
var_mx50 = mean((mx50 - mean_nat).^2)-(biais_mx50)^2;
mean_var50 = var(mx50,1);

biais_medianx50 = mean(medianx50 - mean_nat);
var_medianx50 = mean((medianx50-mean_nat).^2) - (biais_medianx50)^2;
median_var50 = var(medianx50,1);

%di loi student

t = 2.093; %t(1-0.05/2)
for i=1:100
    Sn(:,i) = sqrt((1/(20-1))*(sum(A{i})- mx20(i)).^2);
    li_student(:,i) = mx20(i)-t*Sn(:,i)/(sqrt(20));
    ls_student(:,i) = mx20(i)-t*Sn(:,i)/(sqrt(20));
end

ns = 0;

for i=1:100
    if ((mean_nat>=li_student(:,i)) & (mean_nat<=ls_student(:,i)))
        ns=ns+1;
    end    
end

%dii loi de Gauss

u = 1.96;
    
for i=1:100
    li_gauss(:,i)=mx20(i)-1.96*sqrt(var_mx20);
    ls_gauss(:,i)=mx20(i)+1.96*sqrt(var_mx20);
end


ng= 0;
for i=1:100
    if ((mean_nat>= li_gauss(:,i)) && (mean_nat<= ls_gauss(:,i)))
        ng=ng+1;
    end
end


%%%%%%%%%%%
%Partie: 4%
%%%%%%%%%%%
u = 1.645; % seuil de signification = 5%
n = 40; 
x = 0;

for k=2:100
    if(data(k,1) > data(8,1))
        x=x+1;
    end   
end

p = x/99; % proportion de pays avec taux de natalité inférieur à celui de la Belgique

r = zeros(40,5,100);
A=zeros(40,5,100)
for k=1:100 
    for j=1:5
        r(:,j,k) = randsample(100,40); 
        A(:,j,k) = data(r(:,j,k),1);
    end
end  

nb_pays = zeros(100,5); 

for k=1:100    
    for j=1:5
        for i=1:40
            if(A(i,j,k) > 11.7)
                nb_pays(k,j) = nb_pays(k,j) + 1;
            end
        end
        prop_pays(k,j) = nb_pays(k,j)/40;
    end
end

rejets = 0;
oms = 0;
max_ic = p + u*sqrt((p*(1-p))/n);

for i=1:100
        if(prop_pays(i,1) > max_ic)
          rejets=rejets+1; % Réponse à (a)
        end
    for j=2:5
        if(prop_pays(i,j) > max_ic)
            oms=oms+1; % Réponse à (b)
            break;
        end
    end
end




