%%es1.1
rng(23);
n=5;
A=rand(n,n);
x=ones(n,1);
b=A*x;
N=10;
set=zeros(N,n);
for k=1:N
    set(k,:)=b+rand(n,1);
end
D=diag(rand(1,n));
sol1=A\b;
sol2=D\b;
sol=zeros(N,n);
for k=1:N
    sol(k,:)=A\transpose(set(k,:));
end
n1=norm(A*sol1-b);
n2=norm(D*sol2-b);
normtotal=zeros(1,N);
for k=1:N
    normtotal(k)=norm(A*transpose(sol(k,:))-set(k,:));
end
cond(A)
cond(D)
%%
A=[0.835 0.667;0.333 0.266];
n=11;
b=[0.168, 0.067];
set=zeros(n,2);
for i=1:n
    set(i,:)=b-0.001+(i-1)*0.0002;
end
sol=zeros(n,2);
for i=1:n
    sol(i,:)=A\transpose(set(i,:));
end
normtotal=zeros(1,n);
for i=1:n
    normtotal(i)=norm(A*transpose(sol(i,:))-set(i,:));
end
cond(A)
%%
