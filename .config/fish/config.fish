
for bin in ~/.*/bin
  set -gx PATH $bin $PATH
end

for bin in /opt/*/bin
  set -gx PATH $bin $PATH
end

