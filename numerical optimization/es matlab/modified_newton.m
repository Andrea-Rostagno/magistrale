function [x_min, iter] = modified_newton(f, grad_f, hess_f, x0, tol, max_iter)
    % Input:
    % f: Function handle of the objective function
    % grad_f: Function handle of the gradient
    % hess_f: Function handle of the Hessian
    % x0: Initial guess (vector)
    % tol: Tolerance for convergence
    % max_iter: Maximum number of iterations

    x = x0;
    iter = 0;

    while iter < max_iter
        g = grad_f(x);
        H = hess_f(x);

        % Ensure Hessian is positive definite
        [V, D] = eig(H);
        D(D < 1e-6) = 1e-6; % Modify eigenvalues
        H_mod = V * D * V';

        % Newton direction
        p = -H_mod \ g;

        % Backtracking line search
        alpha = 1;
        rho = 0.5;
        c = 1e-4;
        while f(x + alpha * p) > f(x) + c * alpha * g' * p
            alpha = rho * alpha;
        end

        % Update step
        x = x + alpha * p;

        % Check convergence
        if norm(g) < tol
            break;
        end
        iter = iter + 1;
    end

    x_min = x;
end

% Modified Newton test on Rosenbrock function
clc; clear; close all;

% Define Rosenbrock function
rosenbrock = @(x) 100 * (x(2) - x(1)^2)^2 + (1 - x(1))^2;
grad_rosenbrock = @(x) [ -400*x(1)*(x(2) - x(1)^2) - 2*(1 - x(1));
                          200*(x(2) - x(1)^2)];
hess_rosenbrock = @(x) [ 1200*x(1)^2 - 400*x(2) + 2, -400*x(1);
                         -400*x(1), 200 ];

% Initial points
x0_1 = [1.2, 1.2];
x0_2 = [-1.2, 1.0];

% Parameters
tol = 1e-6;       % Tolerance for convergence
max_iter = 1000;  % Maximum number of iterations

% Run Modified Newton for both starting points
[x_min1, iter1] = modified_newton(rosenbrock, grad_rosenbrock, hess_rosenbrock, x0_1, tol, max_iter);
[x_min2, iter2] = modified_newton(rosenbrock, grad_rosenbrock, hess_rosenbrock, x0_2, tol, max_iter);

% Display results
fprintf('Starting point: [1.2, 1.2]\n');
fprintf('Minimum found: [%f, %f]\n', x_min1(1), x_min1(2));
fprintf('Iterations: %d\n\n', iter1);

fprintf('Starting point: [-1.2, 1.0]\n');
fprintf('Minimum found: [%f, %f]\n', x_min2(1), x_min2(2));
fprintf('Iterations: %d\n\n', iter2);
