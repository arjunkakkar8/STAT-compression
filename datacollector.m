% Run this:
% j = c.batch(@datacollector, 1, {}, 'Pool', 4, 'AttachedFiles', {'sample1.jpg','sample2.jpg','sample3.jpg','sample4.jpg','sample5.jpg',...
% 'sample6.jpg','sample7.jpg','sample8.jpg','sample9.jpg','sample10.jpg'})

function results = datacollector(numsamples, blockruns)

if nargin == 0
    numsamples = 100;
    blockruns = 10;
end

addpath('/Users/arjunkakkar/Desktop/STAT-compression')

% oldvalues = cat(3,...
%     [.5, 0, 1; .8, 0, 1; .2, .5, 1; .5, 0, 2; .2, .5, 2; .5, .5, 2; .8, 1, 2; .2, 0, 3; .8, 0, 3; .2, .5, 3],...
%     [.2, 0, 1; .5, 0, 1; .8, .5, 1; .2, 1, 1; .8, 1, 1; .2, 0, 2; .8, 0, 2; .8, .5, 2; .2, 1, 2; .8, 1, 3],...
%     [.5, 0, 1; .2, .5, 1; .2, .5, 2; .5, 1, 2; .5, 0, 3; .2, .5, 3; .8, .5, 3; .2, 1, 3; .5, 1, 3; .8, 1, 3],...
%     [.2, 0, 1; .8, 0, 1; .8, .5, 1; .2, 1, 1; .5, 1, 1; .5, .5, 2; .8, .5, 2; .2, 1, 2; .2, .5, 3; .5, 1, 3],...
%     [.5, .5, 1; .5, 1, 1; .2, 0, 2; .8, 0, 2; .5, 1, 2; .2, 0, 3; .5, 0, 3; .8, 0, 3; .5, .5, 3; .2, 1, 3],...
%     [.2, .5, 1;.5, .5, 1; .8, .5, 1; .8, 1, 1; .5, 0, 2; .2, 1, 2; .8, 1, 2; .5, .5, 3; .8, .5, 3; .8, 1, 3],...
%     [.8, 0, 1; .5, 1, 1; .2, 0, 2; .2, .5, 2; .5, .5, 2; .5, 1, 2; .2, 0, 3; .5, 0, 3; .8, .5, 3; .5, 1, 3],...
%     [.2, 0, 1; .2, 1, 1; .8, 1, 1; .5, 0, 2; .8, 0, 2; .5, .5, 2; .8, .5, 2; .8, 1, 2; .8, 0, 3; .5, .5, 3],...
%     [.5, 0, 1; .2, .5, 1; .5, .5, 1; .8, .5, 1; .8, 1, 1; .2, 0, 2; .2, 1, 2; .8, 1, 2; .2, 1, 3; .5, 1, 3],...
%     [.2, 0, 1; .8, 0, 1; .5, 1, 1; .8, .5, 2; .5, 1, 2; .2, 0, 3; .8, 0, 3; .2, .5, 3; .5, .5, 3; .2, 1, 3]);


% newvalues = cat(3,...
%     [0.78 0.49 1; 0.80 0.62 1; 0.50 0.78 1; 0.51 0.19 2; 0.54 0.44 2; 0.66 0.79 2; 0.38 0.86 2; 0.58 0.05 3; 0.79 0.11 3; 0.65 0.36 3],...
%     [0.57 0.28 1; 0.36 0.36 1; 0.54 0.77 1; 0.66 0.01 2; 0.57 0.59 2; 0.44 0.69 2; 0.29 0.76 2; 0.46 0.84 2; 0.42 0.26 3; 0.28 0.68 3],...
%     [0.39 0.19 1; 0.20 0.25 1; 0.41 0.58 1; 0.79 0.79 1; 0.53 0.17 2; 0.73 0.38 2;0.52,0.45,2;0.58,0.68,2;0.51,0.11,3;0.28,0.22,3],...
%     [0.25,0.25,1;0.39,0.72,1; 0.63,0.77,1;0.8,0.45,2;0.38,0.66,2;0.42,0.74,2;0.4,0.79,2;0.46,0,3;0.25,0.29,3;0.73,0.32,3],...
%     [0.46,0.04,1;0.4,0.25,1;0.29,0.27,1;0.43,0.8,1;0.78,0.02,2;0.27,0.1,2;0.32,0.84,2;0.71,0.74,3;0.34,0.78,3;0.55,0.81,3],...
%     [0.37,0.53,1;0.69,0.75,1;0.23,0.26,2;0.77,0.46,2;0.25,0.81,2;0.31,0.02,3;0.27,0.22,3;0.58,0.23,3;0.69,0.82,3;0.39,0.87,3],...
%     [0.32,0.34,1;0.38,0.48,1;0.62,0.66,1;0.25,0.13,2;0.78,0.14,2;0.58,0.22,2;0.31,0.33,2;0.65,0.72,2;0.29,0.88,3;0.29,0.9,3],...
%     [0.33,0.1,1;0.4,0.26,1;0.28,0.28,1;0.72,0.42,1;0.23,0.77,1;0.75,0.29,2;0.28,0.58,2;0.58,0.9,2;0.5,0.75,3;0.56,0.87,3],...
%     [0.3,0.32,1;0.49,0.55,1;0.27,0.58,1;0.68,0.62,1;0.21,0.39,2;0.54,0.73,2;0.32,0.82,2;0.68,0.16,3;0.73,0.47,3;0.35,0.8,3],...
%     [0.56,0.04,1;0.53,0.06,1;0.65,0.33,1;0.24,0.56,1;0.78,0.81,1;0.22,0.89,1;0.48,0.63,2;0.62,0.73,2;0.58,0.15,3;0.38,0.72,3]);

values = repmat([0.356,0.899944436534453,1;0.578,0.42540616227298,3;...
0.698,5.55768737231528e-05,1;0.608,0.42008508224854,3;...
0.476,0.443497834356076,3;0.668,0.40944292219966,3;...
0.482,0.442433618351188,3;0.788,5.55768737231528e-05,1;...
0.512,0.437112538326748,3; 0.626,0.416892434233876,3],...
    [1, 1, 10]);

filenames = {'sample1.jpg','sample2.jpg','sample3.jpg','sample4.jpg','sample5.jpg',...
    'sample6.jpg','sample7.jpg','sample8.jpg','sample9.jpg','sample10.jpg'};

mse = zeros(numsamples, 1);
time = zeros(numsamples, 1);

parfor iter = 1:numsamples
    image = ceil(iter/blockruns);
    run = mod(iter-1,blockruns)+1;
    file = char(filenames(image));
    compression = values(run, 1, image);
    edgeprop = values(run, 2, image);
    thickness = values(run, 3, image);
    % 1. Compression
    [origimg, pos] = Compress(file, [2160 ,3800], compression, 'edgeRand', edgeprop, thickness);

    % 2. Decompression
    [reimg, time(iter)] = Decomp(origimg, pos, 'iterative', 2000);

    % 3. Results
    [mse(iter)] = Results(origimg, reimg, pos);
end

% parfor iter = 1:numsamples
%     image = ceil(iter/blockruns);
%     run = mod(iter-1,blockruns)+1;
%     file = char(filenames(image));
%     compression = values(run, 1, image);
%     % 1. Compression
%     [origimg, pos] = Compress(file, [2160 ,3800], compression, 'random');
%     
%     % 2. Decompression
%     [reimg, time(iter)] = Decomp(origimg, pos, 'iterative', 2000);
%     
%     % 3. Results
%     [mse(iter)] = Results(origimg, reimg, pos);
% end


results = [mse, time];

end