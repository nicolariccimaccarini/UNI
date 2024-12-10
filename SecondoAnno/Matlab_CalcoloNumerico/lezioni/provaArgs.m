function [a, b, varargout] = provaArgs(c, d, varargin)
% provaArgs - Function di prova del numero variabile di argomenti

% Controllo sugli input
% ...

% Gestione dei parametri di ingresso

if ( nargin < 2 )
    error('Numero insufficiente di parametri di ingresso');
end

% Valori di default
z = 1;
msg = '';

if ( nargin > 2 )
    if ( ~isnumeric(varargin{1}) )
        error('');
    elseif ( ~isempty(varargin{1}) )
        z = varargin{1};
    end
    if ( nargin > 3 )
        msg = varargin{2};
    end
end 

% Corpo della M-function

a = (c - d).^z;
b = c + d;

if ( ~isempty( msg ) )
    disp( msg );
end

switch nargout
    case 0 
        disp('Calcolo eseguito');
    case {1, 2}
        % non faccio nulla 
    case 3
        varargout{1} = log( abs(c) ) + log( abs(2*d) );
    case 4
        varargout{1} = log( abs(c) ) + log( abs(2*d) );
        varargout{2} = sprintf('Operandi: c = %f, d = %f', c, d);
    otherwise
        warning('Alcuni parametri di uscita non assegnati');
end

end