close all
 


patch_coords = @(minx, maxx, miny, maxy) [ minx maxx maxx minx ; miny miny maxy maxy]';

hold on

v = patch_coords(-8,43,-23,26); fill(v(:,1),v(:,2),'m')

plot(25,3,'xg',"linewidth", 3)
%patch>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
v = patch_coords(13,27,-23,-9); fill(v(:,1),v(:,2),'b')
v = patch_coords(-8,13,-23,-9); fill(v(:,1),v(:,2),'y')
v = patch_coords(-8,13,-9,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(13,27,-9,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(27,43,-23,-9); fill(v(:,1),v(:,2),'y')
v = patch_coords(27,43,-9,26); fill(v(:,1),v(:,2),'y')

plot(25,3,'xg',"linewidth", 3)
%patch>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
v = patch_coords(24,26,-8,-6); fill(v(:,1),v(:,2),'b')
v = patch_coords(-8,13,-23,-9); fill(v(:,1),v(:,2),'m')
v = patch_coords(-8,13,-9,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(13,24,-9,-8); fill(v(:,1),v(:,2),'m')
v = patch_coords(13,24,-8,-6); fill(v(:,1),v(:,2),'m')
v = patch_coords(13,24,-6,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(24,26,-9,-8); fill(v(:,1),v(:,2),'m')
v = patch_coords(24,26,-6,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(26,27,-9,-8); fill(v(:,1),v(:,2),'m')
v = patch_coords(26,27,-8,-6); fill(v(:,1),v(:,2),'m')
v = patch_coords(26,27,-6,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(27,43,-23,-9); fill(v(:,1),v(:,2),'m')
v = patch_coords(27,43,-9,26); fill(v(:,1),v(:,2),'m')

plot(25,3,'xg',"linewidth", 3)
%patch>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
v = patch_coords(12,18,8,14); fill(v(:,1),v(:,2),'b')
v = patch_coords(-8,13,-23,-9); fill(v(:,1),v(:,2),'y')
v = patch_coords(-8,12,-9,8); fill(v(:,1),v(:,2),'y')
v = patch_coords(-8,12,8,14); fill(v(:,1),v(:,2),'y')
v = patch_coords(-8,12,14,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(12,13,-9,8); fill(v(:,1),v(:,2),'y')
v = patch_coords(12,13,14,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(13,24,-9,-8); fill(v(:,1),v(:,2),'y')
v = patch_coords(13,24,-8,-6); fill(v(:,1),v(:,2),'y')
v = patch_coords(13,18,-6,8); fill(v(:,1),v(:,2),'y')
v = patch_coords(13,18,14,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(18,24,-6,8); fill(v(:,1),v(:,2),'y')
v = patch_coords(18,24,8,14); fill(v(:,1),v(:,2),'y')
v = patch_coords(18,24,14,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(24,26,-9,-8); fill(v(:,1),v(:,2),'y')
v = patch_coords(24,26,-6,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(26,27,-9,-8); fill(v(:,1),v(:,2),'y')
v = patch_coords(26,27,-8,-6); fill(v(:,1),v(:,2),'y')
v = patch_coords(26,27,-6,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(27,43,-23,-9); fill(v(:,1),v(:,2),'y')
v = patch_coords(27,43,-9,26); fill(v(:,1),v(:,2),'y')

plot(25,3,'xg',"linewidth", 3)
%patch>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
v = patch_coords(22,30,-6,2); fill(v(:,1),v(:,2),'b')
v = patch_coords(-8,13,-23,-9); fill(v(:,1),v(:,2),'m')
v = patch_coords(-8,12,-9,8); fill(v(:,1),v(:,2),'m')
v = patch_coords(-8,12,8,14); fill(v(:,1),v(:,2),'m')
v = patch_coords(-8,12,14,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(12,13,-9,8); fill(v(:,1),v(:,2),'m')
v = patch_coords(12,13,14,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(13,24,-9,-8); fill(v(:,1),v(:,2),'m')
v = patch_coords(13,22,-8,-6); fill(v(:,1),v(:,2),'m')
v = patch_coords(22,24,-8,-6); fill(v(:,1),v(:,2),'m')
v = patch_coords(13,18,-6,8); fill(v(:,1),v(:,2),'m')
v = patch_coords(13,18,14,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(18,22,-6,2); fill(v(:,1),v(:,2),'m')
v = patch_coords(18,22,2,8); fill(v(:,1),v(:,2),'m')
v = patch_coords(22,24,2,8); fill(v(:,1),v(:,2),'m')
v = patch_coords(18,24,8,14); fill(v(:,1),v(:,2),'m')
v = patch_coords(18,24,14,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(24,26,-9,-8); fill(v(:,1),v(:,2),'m')
v = patch_coords(24,26,2,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(26,27,-9,-8); fill(v(:,1),v(:,2),'m')
v = patch_coords(26,27,-8,-6); fill(v(:,1),v(:,2),'m')
v = patch_coords(26,27,2,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(27,43,-23,-9); fill(v(:,1),v(:,2),'m')
v = patch_coords(27,30,-9,-6); fill(v(:,1),v(:,2),'m')
v = patch_coords(27,30,2,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(30,43,-9,-6); fill(v(:,1),v(:,2),'m')
v = patch_coords(30,43,-6,2); fill(v(:,1),v(:,2),'m')
v = patch_coords(30,43,2,26); fill(v(:,1),v(:,2),'m')

plot(25,3,'xg',"linewidth", 3)
%patch>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
v = patch_coords(26,34,-14,-6); fill(v(:,1),v(:,2),'b')
v = patch_coords(-8,13,-23,-9); fill(v(:,1),v(:,2),'y')
v = patch_coords(-8,12,-9,8); fill(v(:,1),v(:,2),'y')
v = patch_coords(-8,12,8,14); fill(v(:,1),v(:,2),'y')
v = patch_coords(-8,12,14,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(12,13,-9,8); fill(v(:,1),v(:,2),'y')
v = patch_coords(12,13,14,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(13,24,-9,-8); fill(v(:,1),v(:,2),'y')
v = patch_coords(13,22,-8,-6); fill(v(:,1),v(:,2),'y')
v = patch_coords(22,24,-8,-6); fill(v(:,1),v(:,2),'y')
v = patch_coords(13,18,-6,8); fill(v(:,1),v(:,2),'y')
v = patch_coords(13,18,14,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(18,22,-6,2); fill(v(:,1),v(:,2),'y')
v = patch_coords(18,22,2,8); fill(v(:,1),v(:,2),'y')
v = patch_coords(22,24,2,8); fill(v(:,1),v(:,2),'y')
v = patch_coords(18,24,8,14); fill(v(:,1),v(:,2),'y')
v = patch_coords(18,24,14,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(24,26,-9,-8); fill(v(:,1),v(:,2),'y')
v = patch_coords(24,26,2,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(26,27,2,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(27,34,-23,-14); fill(v(:,1),v(:,2),'y')
v = patch_coords(34,43,-23,-14); fill(v(:,1),v(:,2),'y')
v = patch_coords(34,43,-14,-9); fill(v(:,1),v(:,2),'y')
v = patch_coords(27,30,2,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(34,43,-9,-6); fill(v(:,1),v(:,2),'y')
v = patch_coords(30,34,-6,2); fill(v(:,1),v(:,2),'y')
v = patch_coords(34,43,-6,2); fill(v(:,1),v(:,2),'y')
v = patch_coords(30,43,2,26); fill(v(:,1),v(:,2),'y')

plot(25,3,'xg',"linewidth", 3)
%patch>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
v = patch_coords(26,36,-8,2); fill(v(:,1),v(:,2),'b')
v = patch_coords(-8,13,-23,-9); fill(v(:,1),v(:,2),'m')
v = patch_coords(-8,12,-9,8); fill(v(:,1),v(:,2),'m')
v = patch_coords(-8,12,8,14); fill(v(:,1),v(:,2),'m')
v = patch_coords(-8,12,14,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(12,13,-9,8); fill(v(:,1),v(:,2),'m')
v = patch_coords(12,13,14,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(13,24,-9,-8); fill(v(:,1),v(:,2),'m')
v = patch_coords(13,22,-8,-6); fill(v(:,1),v(:,2),'m')
v = patch_coords(22,24,-8,-6); fill(v(:,1),v(:,2),'m')
v = patch_coords(13,18,-6,8); fill(v(:,1),v(:,2),'m')
v = patch_coords(13,18,14,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(18,22,-6,2); fill(v(:,1),v(:,2),'m')
v = patch_coords(18,22,2,8); fill(v(:,1),v(:,2),'m')
v = patch_coords(22,24,2,8); fill(v(:,1),v(:,2),'m')
v = patch_coords(18,24,8,14); fill(v(:,1),v(:,2),'m')
v = patch_coords(18,24,14,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(24,26,-9,-8); fill(v(:,1),v(:,2),'m')
v = patch_coords(24,26,2,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(26,27,2,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(27,34,-23,-14); fill(v(:,1),v(:,2),'m')
v = patch_coords(34,43,-23,-14); fill(v(:,1),v(:,2),'m')
v = patch_coords(34,43,-14,-9); fill(v(:,1),v(:,2),'m')
v = patch_coords(27,30,2,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(34,36,-9,-8); fill(v(:,1),v(:,2),'m')
v = patch_coords(36,43,-9,-8); fill(v(:,1),v(:,2),'m')
v = patch_coords(36,43,-8,-6); fill(v(:,1),v(:,2),'m')
v = patch_coords(36,43,-6,2); fill(v(:,1),v(:,2),'m')
v = patch_coords(30,36,2,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(36,43,2,26); fill(v(:,1),v(:,2),'m')

plot(25,3,'xg',"linewidth", 3)
%patch>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
v = patch_coords(6,24,-8,10); fill(v(:,1),v(:,2),'b')
v = patch_coords(-8,13,-23,-9); fill(v(:,1),v(:,2),'y')
v = patch_coords(-8,6,-9,-8); fill(v(:,1),v(:,2),'y')
v = patch_coords(-8,6,-8,8); fill(v(:,1),v(:,2),'y')
v = patch_coords(6,12,-9,-8); fill(v(:,1),v(:,2),'y')
v = patch_coords(-8,6,8,10); fill(v(:,1),v(:,2),'y')
v = patch_coords(-8,6,10,14); fill(v(:,1),v(:,2),'y')
v = patch_coords(6,12,10,14); fill(v(:,1),v(:,2),'y')
v = patch_coords(-8,12,14,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(12,13,-9,-8); fill(v(:,1),v(:,2),'y')
v = patch_coords(12,13,14,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(13,24,-9,-8); fill(v(:,1),v(:,2),'y')
v = patch_coords(13,18,14,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(18,24,10,14); fill(v(:,1),v(:,2),'y')
v = patch_coords(18,24,14,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(24,26,-9,-8); fill(v(:,1),v(:,2),'y')
v = patch_coords(24,26,2,10); fill(v(:,1),v(:,2),'y')
v = patch_coords(24,26,10,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(26,27,2,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(27,34,-23,-14); fill(v(:,1),v(:,2),'y')
v = patch_coords(34,43,-23,-14); fill(v(:,1),v(:,2),'y')
v = patch_coords(34,43,-14,-9); fill(v(:,1),v(:,2),'y')
v = patch_coords(27,30,2,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(34,36,-9,-8); fill(v(:,1),v(:,2),'y')
v = patch_coords(36,43,-9,-8); fill(v(:,1),v(:,2),'y')
v = patch_coords(36,43,-8,-6); fill(v(:,1),v(:,2),'y')
v = patch_coords(36,43,-6,2); fill(v(:,1),v(:,2),'y')
v = patch_coords(30,36,2,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(36,43,2,26); fill(v(:,1),v(:,2),'y')

plot(25,3,'xg',"linewidth", 3)
%patch>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
v = patch_coords(-8,12,-8,12); fill(v(:,1),v(:,2),'b')
v = patch_coords(-8,13,-23,-9); fill(v(:,1),v(:,2),'m')
v = patch_coords(-8,6,-9,-8); fill(v(:,1),v(:,2),'m')
v = patch_coords(6,12,-9,-8); fill(v(:,1),v(:,2),'m')
v = patch_coords(-8,6,12,14); fill(v(:,1),v(:,2),'m')
v = patch_coords(6,12,12,14); fill(v(:,1),v(:,2),'m')
v = patch_coords(-8,12,14,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(12,13,-9,-8); fill(v(:,1),v(:,2),'m')
v = patch_coords(12,13,14,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(13,24,-9,-8); fill(v(:,1),v(:,2),'m')
v = patch_coords(13,18,14,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(18,24,10,14); fill(v(:,1),v(:,2),'m')
v = patch_coords(18,24,14,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(24,26,-9,-8); fill(v(:,1),v(:,2),'m')
v = patch_coords(24,26,2,10); fill(v(:,1),v(:,2),'m')
v = patch_coords(24,26,10,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(26,27,2,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(27,34,-23,-14); fill(v(:,1),v(:,2),'m')
v = patch_coords(34,43,-23,-14); fill(v(:,1),v(:,2),'m')
v = patch_coords(34,43,-14,-9); fill(v(:,1),v(:,2),'m')
v = patch_coords(27,30,2,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(34,36,-9,-8); fill(v(:,1),v(:,2),'m')
v = patch_coords(36,43,-9,-8); fill(v(:,1),v(:,2),'m')
v = patch_coords(36,43,-8,-6); fill(v(:,1),v(:,2),'m')
v = patch_coords(36,43,-6,2); fill(v(:,1),v(:,2),'m')
v = patch_coords(30,36,2,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(36,43,2,26); fill(v(:,1),v(:,2),'m')

plot(25,3,'xg',"linewidth", 3)
%patch>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
v = patch_coords(8,14,-14,-8); fill(v(:,1),v(:,2),'b')
v = patch_coords(-8,8,-23,-14); fill(v(:,1),v(:,2),'y')
v = patch_coords(-8,8,-14,-9); fill(v(:,1),v(:,2),'y')
v = patch_coords(8,13,-23,-14); fill(v(:,1),v(:,2),'y')
v = patch_coords(-8,6,-9,-8); fill(v(:,1),v(:,2),'y')
v = patch_coords(6,8,-9,-8); fill(v(:,1),v(:,2),'y')
v = patch_coords(-8,6,12,14); fill(v(:,1),v(:,2),'y')
v = patch_coords(6,12,12,14); fill(v(:,1),v(:,2),'y')
v = patch_coords(-8,12,14,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(12,13,14,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(14,24,-9,-8); fill(v(:,1),v(:,2),'y')
v = patch_coords(13,18,14,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(18,24,10,14); fill(v(:,1),v(:,2),'y')
v = patch_coords(18,24,14,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(24,26,-9,-8); fill(v(:,1),v(:,2),'y')
v = patch_coords(24,26,2,10); fill(v(:,1),v(:,2),'y')
v = patch_coords(24,26,10,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(26,27,2,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(27,34,-23,-14); fill(v(:,1),v(:,2),'y')
v = patch_coords(34,43,-23,-14); fill(v(:,1),v(:,2),'y')
v = patch_coords(34,43,-14,-9); fill(v(:,1),v(:,2),'y')
v = patch_coords(27,30,2,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(34,36,-9,-8); fill(v(:,1),v(:,2),'y')
v = patch_coords(36,43,-9,-8); fill(v(:,1),v(:,2),'y')
v = patch_coords(36,43,-8,-6); fill(v(:,1),v(:,2),'y')
v = patch_coords(36,43,-6,2); fill(v(:,1),v(:,2),'y')
v = patch_coords(30,36,2,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(36,43,2,26); fill(v(:,1),v(:,2),'y')

plot(25,3,'xg',"linewidth", 3)
%patch>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
v = patch_coords(26,42,-2,14); fill(v(:,1),v(:,2),'b')
v = patch_coords(-8,8,-23,-14); fill(v(:,1),v(:,2),'m')
v = patch_coords(-8,8,-14,-9); fill(v(:,1),v(:,2),'m')
v = patch_coords(8,13,-23,-14); fill(v(:,1),v(:,2),'m')
v = patch_coords(-8,6,-9,-8); fill(v(:,1),v(:,2),'m')
v = patch_coords(6,8,-9,-8); fill(v(:,1),v(:,2),'m')
v = patch_coords(-8,6,12,14); fill(v(:,1),v(:,2),'m')
v = patch_coords(6,12,12,14); fill(v(:,1),v(:,2),'m')
v = patch_coords(-8,12,14,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(12,13,14,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(14,24,-9,-8); fill(v(:,1),v(:,2),'m')
v = patch_coords(13,18,14,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(18,24,10,14); fill(v(:,1),v(:,2),'m')
v = patch_coords(18,24,14,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(24,26,-9,-8); fill(v(:,1),v(:,2),'m')
v = patch_coords(24,26,2,10); fill(v(:,1),v(:,2),'m')
v = patch_coords(24,26,10,14); fill(v(:,1),v(:,2),'m')
v = patch_coords(24,26,14,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(26,27,14,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(27,34,-23,-14); fill(v(:,1),v(:,2),'m')
v = patch_coords(34,43,-23,-14); fill(v(:,1),v(:,2),'m')
v = patch_coords(34,43,-14,-9); fill(v(:,1),v(:,2),'m')
v = patch_coords(27,30,14,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(34,36,-9,-8); fill(v(:,1),v(:,2),'m')
v = patch_coords(36,43,-9,-8); fill(v(:,1),v(:,2),'m')
v = patch_coords(36,43,-8,-6); fill(v(:,1),v(:,2),'m')
v = patch_coords(36,42,-6,-2); fill(v(:,1),v(:,2),'m')
v = patch_coords(42,43,-6,-2); fill(v(:,1),v(:,2),'m')
v = patch_coords(42,43,-2,2); fill(v(:,1),v(:,2),'m')
v = patch_coords(30,36,14,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(36,42,14,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(42,43,2,14); fill(v(:,1),v(:,2),'m')
v = patch_coords(42,43,14,26); fill(v(:,1),v(:,2),'m')

plot(25,3,'xg',"linewidth", 3)
%patch>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
v = patch_coords(31,43,-9,3); fill(v(:,1),v(:,2),'b')
v = patch_coords(-8,8,-23,-14); fill(v(:,1),v(:,2),'y')
v = patch_coords(-8,8,-14,-9); fill(v(:,1),v(:,2),'y')
v = patch_coords(8,13,-23,-14); fill(v(:,1),v(:,2),'y')
v = patch_coords(-8,6,-9,-8); fill(v(:,1),v(:,2),'y')
v = patch_coords(6,8,-9,-8); fill(v(:,1),v(:,2),'y')
v = patch_coords(-8,6,12,14); fill(v(:,1),v(:,2),'y')
v = patch_coords(6,12,12,14); fill(v(:,1),v(:,2),'y')
v = patch_coords(-8,12,14,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(12,13,14,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(14,24,-9,-8); fill(v(:,1),v(:,2),'y')
v = patch_coords(13,18,14,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(18,24,10,14); fill(v(:,1),v(:,2),'y')
v = patch_coords(18,24,14,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(24,26,-9,-8); fill(v(:,1),v(:,2),'y')
v = patch_coords(24,26,2,10); fill(v(:,1),v(:,2),'y')
v = patch_coords(24,26,10,14); fill(v(:,1),v(:,2),'y')
v = patch_coords(24,26,14,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(26,27,14,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(27,34,-23,-14); fill(v(:,1),v(:,2),'y')
v = patch_coords(34,43,-23,-14); fill(v(:,1),v(:,2),'y')
v = patch_coords(34,43,-14,-9); fill(v(:,1),v(:,2),'y')
v = patch_coords(27,30,14,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(30,36,14,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(36,42,14,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(42,43,3,14); fill(v(:,1),v(:,2),'y')
v = patch_coords(42,43,14,26); fill(v(:,1),v(:,2),'y')

plot(25,3,'xg',"linewidth", 3)
%patch>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
v = patch_coords(18,28,4,14); fill(v(:,1),v(:,2),'b')
v = patch_coords(-8,8,-23,-14); fill(v(:,1),v(:,2),'m')
v = patch_coords(-8,8,-14,-9); fill(v(:,1),v(:,2),'m')
v = patch_coords(8,13,-23,-14); fill(v(:,1),v(:,2),'m')
v = patch_coords(-8,6,-9,-8); fill(v(:,1),v(:,2),'m')
v = patch_coords(6,8,-9,-8); fill(v(:,1),v(:,2),'m')
v = patch_coords(-8,6,12,14); fill(v(:,1),v(:,2),'m')
v = patch_coords(6,12,12,14); fill(v(:,1),v(:,2),'m')
v = patch_coords(-8,12,14,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(12,13,14,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(14,24,-9,-8); fill(v(:,1),v(:,2),'m')
v = patch_coords(13,18,14,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(18,24,14,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(24,26,-9,-8); fill(v(:,1),v(:,2),'m')
v = patch_coords(24,26,2,4); fill(v(:,1),v(:,2),'m')
v = patch_coords(24,26,14,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(26,27,14,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(27,34,-23,-14); fill(v(:,1),v(:,2),'m')
v = patch_coords(34,43,-23,-14); fill(v(:,1),v(:,2),'m')
v = patch_coords(34,43,-14,-9); fill(v(:,1),v(:,2),'m')
v = patch_coords(27,28,14,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(28,30,14,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(30,36,14,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(36,42,14,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(42,43,3,14); fill(v(:,1),v(:,2),'m')
v = patch_coords(42,43,14,26); fill(v(:,1),v(:,2),'m')

plot(25,3,'xg',"linewidth", 3)
%patch>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
v = patch_coords(16,18,10,12); fill(v(:,1),v(:,2),'b')
v = patch_coords(-8,8,-23,-14); fill(v(:,1),v(:,2),'y')
v = patch_coords(-8,8,-14,-9); fill(v(:,1),v(:,2),'y')
v = patch_coords(8,13,-23,-14); fill(v(:,1),v(:,2),'y')
v = patch_coords(-8,6,-9,-8); fill(v(:,1),v(:,2),'y')
v = patch_coords(6,8,-9,-8); fill(v(:,1),v(:,2),'y')
v = patch_coords(-8,6,12,14); fill(v(:,1),v(:,2),'y')
v = patch_coords(6,12,12,14); fill(v(:,1),v(:,2),'y')
v = patch_coords(-8,12,14,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(12,13,14,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(14,24,-9,-8); fill(v(:,1),v(:,2),'y')
v = patch_coords(13,18,14,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(18,24,14,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(24,26,-9,-8); fill(v(:,1),v(:,2),'y')
v = patch_coords(24,26,2,4); fill(v(:,1),v(:,2),'y')
v = patch_coords(24,26,14,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(26,27,14,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(27,34,-23,-14); fill(v(:,1),v(:,2),'y')
v = patch_coords(34,43,-23,-14); fill(v(:,1),v(:,2),'y')
v = patch_coords(34,43,-14,-9); fill(v(:,1),v(:,2),'y')
v = patch_coords(27,28,14,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(28,30,14,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(30,36,14,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(36,42,14,26); fill(v(:,1),v(:,2),'y')
v = patch_coords(42,43,3,14); fill(v(:,1),v(:,2),'y')
v = patch_coords(42,43,14,26); fill(v(:,1),v(:,2),'y')

plot(25,3,'xg',"linewidth", 3)
%patch>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
v = patch_coords(14,28,12,26); fill(v(:,1),v(:,2),'b')
v = patch_coords(-8,8,-23,-14); fill(v(:,1),v(:,2),'m')
v = patch_coords(-8,8,-14,-9); fill(v(:,1),v(:,2),'m')
v = patch_coords(8,13,-23,-14); fill(v(:,1),v(:,2),'m')
v = patch_coords(-8,6,-9,-8); fill(v(:,1),v(:,2),'m')
v = patch_coords(6,8,-9,-8); fill(v(:,1),v(:,2),'m')
v = patch_coords(-8,6,12,14); fill(v(:,1),v(:,2),'m')
v = patch_coords(6,12,12,14); fill(v(:,1),v(:,2),'m')
v = patch_coords(-8,12,14,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(12,13,14,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(14,24,-9,-8); fill(v(:,1),v(:,2),'m')
v = patch_coords(13,14,14,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(24,26,-9,-8); fill(v(:,1),v(:,2),'m')
v = patch_coords(24,26,2,4); fill(v(:,1),v(:,2),'m')
v = patch_coords(27,34,-23,-14); fill(v(:,1),v(:,2),'m')
v = patch_coords(34,43,-23,-14); fill(v(:,1),v(:,2),'m')
v = patch_coords(34,43,-14,-9); fill(v(:,1),v(:,2),'m')
v = patch_coords(28,30,14,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(30,36,14,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(36,42,14,26); fill(v(:,1),v(:,2),'m')
v = patch_coords(42,43,3,14); fill(v(:,1),v(:,2),'m')
v = patch_coords(42,43,14,26); fill(v(:,1),v(:,2),'m')