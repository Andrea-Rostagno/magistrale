% Definire i limiti dell'intervallo
a = 2; % Limite inferiore
b = 5; % Limite superiore

% Generare un valore casuale nell'intervallo [a, b]
x = a + (b - a) * rand;

% Visualizza il risultato
disp(['Punto casuale generato: ', num2str(x)]);


% Definire i limiti dell'intervallo
a = 2; % Limite inferiore
b = 5; % Limite superiore

% Numero di punti
n = 10;

% Generare n valori casuali nell'intervallo [a, b]
points = a + (b - a) * rand(1, n);

% Visualizza i risultati
disp('Punti casuali generati:');
disp(points);
