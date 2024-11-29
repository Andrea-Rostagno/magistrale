A=[0.2, 0.1, 0.1, 0;
    0.1, 0.3, 0, 0;
    0.1, 0, 0.2, 0.1;
    0, 0, 0.1, 0.3];

A_inv = inv(A);
disp(A_inv)

I=ones(1,4);

denominatore=I * A_inv * I';
disp(denominatore);

numeratore=A_inv * I';
disp(numeratore);

tot=numeratore/denominatore;
