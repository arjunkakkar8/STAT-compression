function pos = Init(origimg, ratio, method, propedge, inflation)

origSize = numel(origimg);
compSize = floor(origSize*ratio);

% Select initial points based on called method
switch method
    % randomly select initial points
    case 'random'
        % pos = datasample(1:origSize, compSize, 'Replace', false);
        
        pos = randperm(origSize, compSize);
        
        % randomly select edge points and non-edge points
        % given proportion propedge (default to 0.2)
    case 'edgeRand'
        contour = edge(origimg', 'Roberts', 20);
        contour = imdilate(contour, strel('disk', inflation));
        edges = find(contour)';
        edgeSize = min(numel(edges),floor(propedge .* compSize));
        if numel(edges)~=0
            %edges = datasample(edges, min(compSize, edgeSize), 'Replace', false);
            edges = edges(randperm(numel(edges), min(compSize, edgeSize)));
        else
            edges = [];
            disp('No edges detected')
        end
        %points = datasample(1:numel(origimg), max(0, compSize-edgeSize), 'Replace', false);
        
        points = randperm(origSize, max(0, compSize-edgeSize));
        
        pos = [edges points];
        
        % evenly spaced grid
        % case 'grid'
        
end

end
