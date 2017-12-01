addpath('/Users/ak23/PDE-compression')
addpath('/Users/ak23/STAT-compression')
results = zeros(100, 2);

values = cat(3,...
    [.5, 0, 1; .8, 0, 1; .2, .5, 1; .5, 0, 2; .2, .5, 2; .5, .5, 2; .8, 1, 2; .2, 0, 3; .8, 0, 3; .2, .5, 3],...
    [.2, 0, 1; .5, 0, 1; .8, .5, 1; .2, 1, 1; .8, 1, 1; .2, 0, 2; .8, 0, 2; .8, .5, 2; .2, 1, 2; .8, 1, 3],...
    [.5, 0, 1; .2, .5, 1; .2, .5, 2; .5, 1, 2; .5, 0, 3; .2, .5, 3; .8, .5, 3; .2, 1, 3; .5, 1, 3; .8, 1, 3],...
    [.2, 0, 1; .8, 0, 1; .8, .5, 1; .2, 1, 1; .5, 1, 1; .5, .5, 2; .8, .5, 2; .2, 1, 2; .2, .5, 3; .5, 1, 3],...
    [.5, .5, 1; .5, 1, 1; .2, 0, 2; .8, 0, 2; .5, 1, 2; .2, 0, 3; .5, 0, 3; .8, 0, 3; .5, .5, 3; .2, 1, 3],...
    [.2, .5, 1;.5, .5, 1; .8, .5, 1; .8, 1, 1; .5, 0, 2; .2, 1, 2; .8, 1, 2; .5, .5, 3; .8, .5, 3; .8, 1, 3],...
    [.8, 0, 1; .5, 1, 1; .2, 0, 2; .2, .5, 2; .5, .5, 2; .5, 1, 2; .2, 0, 3; .5, 0, 3; .8, .5, 3; .5, 1, 3],...
    [.2, 0, 1; .2, 1, 1; .8, 1, 1; .5, 0, 2; .8, 0, 2; .5, .5, 2; .8, .5, 2; .8, 1, 2; .8, 0, 3; .5, .5, 3],...
    [.5, 0, 1; .2, .5, 1; .5, .5, 1; .8, .5, 1; .8, 1, 1; .2, 0, 2; .2, 1, 2; .8, 1, 2; .2, 1, 3; .5, 1, 3],...
    [.2, 0, 1; .8, 0, 1; .5, 1, 1; .8, .5, 2; .5, 1, 2; .2, 0, 3; .8, 0, 3; .2, .5, 3; .5, .5, 3; .2, 1, 3]);

filenames = {'sample1.jpg','sample2.jpg','sample3.jpg','sample4.jpg','sample5.jpg',...
    'sample6.jpg','sample7.jpg','sample8.jpg','sample9.jpg','sample10.jpg'};

for j = 1:10
    file = char(filenames(j));
    parfor i = 1:10
        compression = values(i, 1, j);
        edgeprop = values(i, 2, j);
        thickness = values(i, 3, j);
        
        % 1. Compression
        [origimg, pos] = Compress(file, [2160 ,3800], compression, 'edgeRand', edgeprop, thickness);
        
        % 2. Decompression
        [reimg, innerresults2(i)] = Decomp(origimg, pos, 'iterative', 2000);
        
        % 3. Results
        [innerresults1(i)] = Results(origimg, reimg, pos);
        %drawnow
        disp(j)
        disp(i)
    end
    results((j-1)*10+1:j*10, 1) = innerresults1;
    results((j-1)*10+1:j*10, 2) = innerresults2;
end