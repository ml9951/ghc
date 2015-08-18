function [] = time_stats(events, tags)

times = cell(size(events));
for i = 1:length(events)
    core = events{i};
    startStops = core(core(:, tags.Type) >= 0 | core(:, tags.Type) == tags.Commit, :);
    
    times{i} = [startStops(1:2:end, tags.Type), startStops(2:2:end, 1) - startStops(1:2:end, 1)];
    
end

header = 'Core  |  ';
types = unique(times{1}(:, 1));

for i = types'
    header = [header 'Operation ' num2str(i) '  |  ' ];
end

fprintf([header '\n']);


end