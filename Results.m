function [absdist] = Results(origimg, reimg, pos, baseline)


if nargin < 4
    baseline = false;
end

if baseline == true
    % Compare to the baseline best methods available in MATLAB
    % Create x, y values for the positions in the matrix
    [x ,y] = ind2sub(size(origimg), pos);
    [xq ,yq] = ind2sub(size(origimg), 1:numel(origimg));
    % Interpolate using MATLAB's inbuilt methods. 'v4' works better than all
    % other methods including linear, nearest neighbour and cubic.
    interp = griddata(x', y', origimg(pos)', xq', yq', 'cubic');
    baseline = reshape(interp, size(origimg, 1), size(origimg, 2));
    
    basemse = sqrt(mean2((mat2gray(origimg)-mat2gray(baseline)).^2));  % MSE
    baseabsdist = mean2(abs(mat2gray(origimg)-mat2gray(baseline)));    % Absolute Distance
    
    basemsg = ['The distance between the MATLAB interpolation and the original image is - MSE: ',...
        num2str(basemse), ', Absolute Distance: ', num2str(baseabsdist),'.'];
    disp(basemsg)
end
% Compute closeness metrics
mse = sqrt(mean2((mat2gray(origimg)-mat2gray(reimg)).^2));  % MSE
absdist = mean2(abs(mat2gray(origimg)-mat2gray(reimg)));    % Absolute Distance

% Closeness message
% closenessmsg = ['The distance between the reconstructed image and the original image is - MSE: ',...
%     num2str(mse), ', Absolute Distance: ', num2str(absdist),'.'];
% disp(closenessmsg)
% 
% allpoints = zeros(size(origimg))';
% allpoints(pos)=origimg(pos);

% Display the results of the procedure
% subplot(2, 2, 1)
% imshow(mat2gray(origimg))
% title('Original Image')
% subplot(2, 2, 2)
% imshow(mat2gray(reimg))
% title('Compressed Image')
% subplot(2, 2, 3)
% imshow(mat2gray(abs(origimg-reimg)));
% title('Difference between the Original and Compressed Image')
% subplot(2, 2, 4)
% imshow(allpoints')
% title('All the initial points')

% subplot(3, 2, 5)
% imshow(mat2gray(baseline))
% title('MATLAB Interpolation')
% subplot(3, 2, 6)
% imshow(mat2gray(abs(origimg-baseline)));
% title('Difference between the Original and MATLAB Interpolation')

end