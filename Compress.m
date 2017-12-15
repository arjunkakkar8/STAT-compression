% compress image based on called method
function [origimg, pos] = Compress(file, dims, ratio, method, propedge, inflation)
% Set proportion of edge points to be 0.2 by default
if nargin < 5
    propedge = 0.2;
end

if nargin < 6
    inflation = 1;
end

% Import image
img = imread(file);
% Store greyvalues of image
origimg = double(rgb2gray(img));
origimg = origimg(1:dims(1), 1:dims(2));
% Choose initial points and store initial values
pos = Init(origimg, ratio, method, propedge, inflation);
end
