function [x_min, f_min, iter] = nelder_mead(f, x0, tol, max_iter)
    % Input:
    % f: Function handle of the objective function
    % x0: Initial guess (vector)
    % tol: Tolerance for convergence
    % max_iter: Maximum number of iterations

    n = length(x0); % Dimension of the problem
    alpha = 1;      % Reflection coefficient
    gamma = 2;      % Expansion coefficient
    rho = 0.5;      % Contraction coefficient
    sigma = 0.5;    % Shrink coefficient

    % Initialize simplex
    simplex = zeros(n + 1, n);
    simplex(1, :) = x0;
    identity_matrix = eye(n); % Matrice identità
    for i = 2:n+1
        simplex(i, :) = x0 + 0.05 * identity_matrix(:, i-1)'; % Small perturbations
    end

    % Evaluate function at simplex points
    f_vals = arrayfun(@(i) f(simplex(i, :)), 1:n+1);

    iter = 0;
    while iter < max_iter
        % Sort vertices by function values
        [f_vals, idx] = sort(f_vals);
        simplex = simplex(idx, :);

        % Compute centroid of all points except worst
        centroid = mean(simplex(1:n, :));

        % Reflection
        x_r = centroid + alpha * (centroid - simplex(end, :));
        f_r = f(x_r);

        if f_r < f_vals(1) % Expansion
            x_e = centroid + gamma * (x_r - centroid);
            f_e = f(x_e);
            if f_e < f_r
                simplex(end, :) = x_e;
                f_vals(end) = f_e;
            else
                simplex(end, :) = x_r;
                f_vals(end) = f_r;
            end
        elseif f_r < f_vals(end-1) % Accept reflection
            simplex(end, :) = x_r;
            f_vals(end) = f_r;
        else % Contraction
            x_c = centroid + rho * (simplex(end, :) - centroid);
            f_c = f(x_c);
            if f_c < f_vals(end)
                simplex(end, :) = x_c;
                f_vals(end) = f_c;
            else % Shrink
                for i = 2:n+1
                    simplex(i, :) = simplex(1, :) + sigma * (simplex(i, :) - simplex(1, :));
                    f_vals(i) = f(simplex(i, :));
                end
            end
        end

        % Check convergence
        if std(f_vals) < tol
            break;
        end
        iter = iter + 1;
    end

    x_min = simplex(1, :);
    f_min = f_vals(1);
end

% Nelder-Mead test on Rosenbrock function
clc; clear; close all;

% Define Rosenbrock function
rosenbrock = @(x) 100 * (x(2) - x(1)^2)^2 + (1 - x(1))^2;

% Initial points
x0_1 = [1.2, 1.2];
x0_2 = [-1.2, 1.0];

% Parameters
tol = 1e-6;       % Tolerance for convergence
max_iter = 1000;  % Maximum number of iterations

% Run Nelder-Mead for both starting points
[x_min1, f_min1, iter1] = nelder_mead(rosenbrock, x0_1, tol, max_iter);
[x_min2, f_min2, iter2] = nelder_mead(rosenbrock, x0_2, tol, max_iter);

% Display results
fprintf('Starting point: [1.2, 1.2]\n');
fprintf('Minimum found: [%f, %f]\n', x_min1(1), x_min1(2));
fprintf('Function value: %f\n', f_min1);
fprintf('Iterations: %d\n\n', iter1);

fprintf('Starting point: [-1.2, 1.0]\n');
fprintf('Minimum found: [%f, %f]\n', x_min2(1), x_min2(2));
fprintf('Function value: %f\n', f_min2);
fprintf('Iterations: %d\n\n', iter2);
