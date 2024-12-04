x0_new = random_points(i,:);

    [x_min, f_min, iter, min_history] = nelder_mead(chained_rosenbrock, x0_new, tol, max_iter);
    
    % Display results
    fprintf('*** Nelder-Mead result dimension %d***\n',j);
    fprintf('Starting point: Solution %d\n', i);
    fprintf('Minimum found: [%f, %f]\n', x_min(1), x_min(2));
    fprintf('Function value: %f\n', f_min);
    fprintf('Iterations: %d\n\n', iter);

    % Plot figures
    iterations_1=0:iter-1;

    figure;
    display_name = sprintf('Solution %d', i);
    plot(iterations_1, min_history, '-o', 'DisplayName', display_name);
    xlabel('Numero di Iterazioni');
    ylabel('Valore della Funzione Obiettivo');
    title('Convergenza Nelder-Mead Funzione di Chained Rosenbrock');
    legend show;
    grid on;