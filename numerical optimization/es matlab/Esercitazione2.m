function [x,kmax,ier]= lab02_gradient_linsys(A,b,x,tol,kmax)
ier=0;
r=b-A*x;
for k=1:kmax
    alpha=r'*r/(r'*(A*r));
    x=x+alpha*r;
    r=r-alpha*(A*r);
    if norm(r)<=tol*norm(b)
        ier=1;
        break
    end
end

end

tol=0.001;
[xsol1,kmax1,ier1]=lab02_gradient_linsys(A,b,x0,tol,n);

function [x,kmax,ier]= lab02_cg_linsys(A,b,x,tol,kmax)
ier=0;
r=b-A*x;
p=r;
for k=1:kmax
    alpha=r'*r/(r'*(A*r));
    x=x+alpha*p;
    r=b-A*x;
    beta=-(p'*(A*r))/(p'*(A*p));
    p=r+beta*p;
    if norm(r)<=tol*norm(b)
        ier=1;
        break
    end
end

end

[xsol2,kmax2,ier2]=lab02_cg_linsys(A,b,x0,tol,n);
%%
n=1000;
d1=4*ones(1,n);
d2=2*ones(1,n);
d=-1*ones(1,n);
A1=spdiags([d,d1,d],[-1,0,1],n,n);
A2=spdiags([d,d2,d],[-1,0,1],n,n);
display("primo numero cond "+condest(A1))
display("secondo numero cond "+condest(A2))



