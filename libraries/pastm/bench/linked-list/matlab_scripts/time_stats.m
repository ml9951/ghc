function [times, allCores] = time_stats(events, tags, tagMapping)

times = cell(size(events));
for i = 1:length(events)
    core = events{i};
    startStops = core(core(:, tags.Type) >= 0 | core(:, tags.Type) == tags.Commit, :);
    times{i} = [startStops(1:2:end, tags.Type), ...
                startStops(2:2:end, 1) - startStops(1:2:end, 1), ...
                startStops(1:2:end, tags.Time), ...
                zeros(size(startStops, 1) / 2, 1) + i];
end

allCores = cell2mat(times);

if nargin > 2
    labels = tagMapping(allCores(:, 1), :);
else
    labels = allCores(:, 1);
end

boxplot(allCores(:, 2) / 1000000, labels);
ylabel('Time in Milliseconds')
end