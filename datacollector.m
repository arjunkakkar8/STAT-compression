mse = zeros(11, 2);
for j = 1:2
    for i = 0:10
        % 1. Compression
        [origimg, pos] = Compress('anotherimg.jpg',[100 ,100], .2, 'edgeRand', .1*i, 2);
        
        % 2. Decompression
        reimg = Decomp(origimg, pos, 'explicit');
        
        % Processing
        % Empty step for now
        
        % 3. Results
        mse(i+1, j) = Results(origimg, reimg, pos);
        %drawnow
    end
end