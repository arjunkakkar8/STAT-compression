% The function decompresses the image and reconstructs an image using the
% stored points.

% The method argument can be used to either decompress using an explicit
% solution or iteratively.

function [reimg, stop] = Decomp(origimg, pos, method, maxiter, threshold, anisotype, K)

if nargin < 4
    maxiter = 500;
end

if nargin < 5
    threshold = 1e-3;
end

switch method
    
    case 'explicit'
        
        width = size(origimg, 2);  % Must be greater than 2
        height = size(origimg, 1); % Must be greated than 2
        N = width * height;
        
        % Random greyvalues for now
        f = reshape(origimg',1,N);
        
        % Generate confidence vector of length N with n 1's
        c = zeros(1, N);
        c(pos) = 1;
        
        % Confidence diagonal matrix from c
        C = spdiags(c', 0, N, N);
        
        % Generate the Laplacian operator matrix A
        hx = 1;
        hy = 1;
        
        % Define A matrix by defining 3 diagonals
        
        % First define the main diagonal
        maindiag = [-1./(hx)^2-1./(hy)^2,...
            repmat(-2./(hx)^2-1./(hy)^2, [1, width-2]),...
            -1./(hx)^2-1./(hy)^2,...
            repmat([-1./(hx)^2-2./(hy)^2,...
            repmat(-2./(hx)^2-2./(hy)^2, [1, width-2]),...
            -1./(hx)^2-2./(hy)^2], [1,height-2]),...
            -1./(hx)^2-1./(hy)^2,...
            repmat(-2./(hx)^2-1./(hy)^2, [1, width-2]),...
            -1./(hx)^2-1./(hy)^2];
        
        % Define the diagonal with x-adjacencies
        xdiag = repmat([repmat(1./(hx)^2, [1,width-1]), 0], [1, height]);
        xdiagmod = repmat([0, repmat(1./(hx)^2, [1,width-1])], [1, height]);
        
        % Define the diagonal with y-adjacencies
        ydiag = repmat(1./(hy)^2, [1, N]);
        
        % Create the A matrix using these diagonals
        A = spdiags([ydiag' xdiag' maindiag' xdiagmod' ydiag'],...
            [-width, -1, 0, 1, width], N, N);
        
        % Compute Mext
        Mext = C - (speye(N) - C) * A;
        
        
        disp('Computing the inverse...')
        start = tic;
        % Compute u = inverse Mext * C * f'
        u = Mext\(C * f.');
        stop = toc(start);
        
        disp('Time taken for inverse is')
        disp(stop)
        
        % Create image matrix
        reimg = reshape(u, width, height)';
        
    case 'iterative'
        
        origimg = origimg';
        
        Nx = size(origimg, 1);
        Ny = size(origimg, 2);
        
        reimg = rand(Nx, Ny)*255;       % Start with random values
        reimg(pos) = origimg(pos);      % Insert the values from the compressed vector
        
        % Initialize the values matrix that stores the image information at every
        % timestep
        h = 2;  % This is technically a combination of h and timestep t
        values = zeros(Nx+2, Ny+2, 2);  % Create a N+2 by N+2 gird
        values(2:Nx+1,2:Ny+1,2) = reimg;    % Add values in the inner N by N grid
        
        % Add values in the boundary to implement the Neumann condition
        values(2:Nx+1, 1, 2) = values(2:Nx+1, 2, 2);            % Left Boundary
        values(2:Nx+1, Ny+2, 2) = values(2:Nx+1, Ny+1, 2);      % Right Boundary
        values(1, 2:Ny+1, 2) = values(2, 2:Ny+1, 2);            % Top Boundary
        values(Nx+2, 2:Ny+1, 2) = values(Nx+1, 2:Ny+1, 2);      % Bottom Boundary
        
        disp('Iterating...')
        start = tic;
        for iter = 1:maxiter
            % Compute the next iteration using finite difference form of laplacian
            nextiter = values(:,:,2) + (1/h.^2).*(circshift(values(:,:,2), [1,0])...
                +circshift(values(:,:,2), [-1,0])+circshift(values(:,:,2), [0,1])...
                +circshift(values(:,:,2), [0,-1])-4.*values(:,:,2));
            % Enforce the girdpoints we know again
            inner = nextiter(2:Nx+1, 2:Ny+1);
            inner(pos) = origimg(pos);
            nextiter(2:Nx+1, 2:Ny+1) = inner;
            % Enforce the boundary again
            nextiter(2:Nx+1, 1) = nextiter(2:Nx+1, 2);            % Left Boundary
            nextiter(2:Nx+1, Ny+2) = nextiter(2:Nx+1, Ny+1);      % Right Boundary
            nextiter(1, 2:Ny+1) = nextiter(2, 2:Ny+1);            % Top Boundary
            nextiter(Nx+2, 2:Ny+1) = nextiter(Nx+1, 2:Ny+1);      % Bottom Boundary
            
            values(:,:,1)=values(:,:,2); % Store last values
            values(:,:,2)=nextiter;         % Update values
            disp(iter);
            convergence=sqrt(mean2((values(2:Nx+1,2:Ny+1,2)-values(2:Nx+1,2:Ny+1,1)).^2));
            if convergence < threshold
                break
            end
        end
        stop = toc(start);
        disp('Time taken to complete convergence is')
        disp(stop)
        % Display a message with information from the iterative process
        msg = ['The convergence metric is ', num2str(convergence), '.'];
        disp(msg)
        % Store the values from the completion of the iterations
        reimg = values(2:Nx+1, 2:Ny+1,2)';
        
    case 'aniso'
        reimg = aniso(origimg, pos, maxiter, anisotype, K);
        
end
