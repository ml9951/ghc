
%Note: times are in nano seconds
files = dir('*.mat');
for file = files'
    load(file.name)
    events(:, 2) = events(:, 2) + 1;
    sizes = zeros(size(events(:, 3)));
    sizes(events(:, 3) > 4) = bitshift(events(events(:, 3) > 4, 3), -34);
    items = zeros(size(events(:, 3)));
    mask = uint64(17179869183);
    items(events(:, 3) > 4) = bitshift(bitand(uint64(events(events(:, 3) > 4, 3)), mask), -4);
    events(events(:, 3) > 4, 3) = bitand(uint64(events(events(:, 3) > 4, 3)), 15);
    events = [events, items, sizes];
    cores = unique(events(:, 2))';
    name = file.name(1:end-4);
    eval([name 'Events = cell(length(cores), 1);'])
    for c = cores
        eval([name 'Events{c} = events(events(:, 2) == c, [1, 3, 4, 5]);']);
    end
    tags = struct('Commit', Commit, 'CommitFA', CommitFA, 'CommitPA', CommitPA, ...
                 'EagerFA', EagerFA, 'EagerPA', EagerPA, 'Validate', Validate, ...
                 'Time', 1, 'Type', 2, 'Item', 'Size');
    eval([name 'Tags = tags;']);
    eval(['sanity_check(' name 'Events, ' name 'Tags);']);
    clear events c Commit CommitFA CommitPA EagerFA EagerPA cores Validate name tags
end

clear file files ans sizes items mask








