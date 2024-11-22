function [x_min, f_min, iter, min_history] = modified_newton(f, grad_f, hess_f, x0, tol, max_iter)
    % Input:
    % f: Function handle of the objective function
    % grad_f: Function handle of the gradient
    % hess_f: Function handle of the Hessian
    % x0: Initial guess (vector)
    % tol: Tolerance for convergence
    % max_iter: Maximum number of iterations

    x = x0;
    iter = 0;
    n = length(x0);
    min_history=[f(x0)];

    while iter < max_iter
        g = grad_f(x);
        H = hess_f(x);

        % Define Hessian matrix modified
        tao = max( 0 , sqrt(eps) - min(eig(H)) );
        H_mod= H + tao*eye(n);

        % Newton direction
        p = -H_mod \ g;

        % Backtracking line search
        alpha = 1;
        rho = 0.5;
        c = 1e-4;
        max_backtracking_iter = 20; % Limita la ricerca lineare
        backtracking_iter = 0;
        while f(x + alpha * p) > f(x) + c * alpha * g' * p && backtracking_iter < max_backtracking_iter
            alpha = rho * alpha;
            backtracking_iter = backtracking_iter + 1;
        end

        % Update step
        x = x + alpha * p;

        % Check convergence
        if norm(g) < tol
            break;
        end
        iter = iter + 1;
        min_history=[min_history,f(x)];
    end

    x_min = x;
    f_min = f(x);
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
[x_min1, f_min1, iter1, min_history1] = modified_newton(rosenbrock, grad_rosenbrock, hess_rosenbrock, x0_1, tol, max_iter);
[x_min2, f_min2, iter2, min_history2] = modified_newton(rosenbrock, grad_rosenbrock, hess_rosenbrock, x0_2, tol, max_iter);

% Display results
fprintf('Starting point: [1.2, 1.2]\n');
fprintf('Minimum found: [%f, %f]\n', x_min1(1), x_min1(2));
fprintf('Function value: %f\n', f_min1);
fprintf('Iterations: %d\n\n', iter1);

fprintf('Starting point: [-1.2, 1.0]\n');
fprintf('Minimum found: [%f, %f]\n', x_min2(1), x_min2(2));
fprintf('Function value: %f\n', f_min2);
fprintf('Iterations: %d\n\n', iter2);

% Plot figures
iterations_1=0:iter1;
iterations_2=0:iter2;

figure;
plot(iterations_1, min_history1, '-o', 'DisplayName', '[1.2, 1.2]');
hold on;
plot(iterations_2, min_history2, '-x', 'DisplayName', '[-1.2, 1.0]');
hold off;
xlabel('Numero di Iterazioni');
ylabel('Valore della Funzione Obiettivo');
title('Convergenza del Metodo Nelder-Mead sulla Funzione di Rosenbrock');
legend show;
grid on;