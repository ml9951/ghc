

files = dir('*.mat');
for file = files'
    load(file.name)
    Xevents(:, 2) = Xevents(:, 2) + 1;
    cores = unique(Xevents(:, 2))';
    name = file.name(1:end-4);
    eval([name 'Events = cell(length(cores), 1);'])
    for c = cores
        eval([name 'Events{c} = Xevents(Xevents(:, 3) == c, :);']);
    end
    tags = struct('Commit', Commit, 'CommitFA', CommitFA, 'CommitPA', CommitPA, ...
                 'EagerFA', EagerFA, 'EagerPA', EagerPA, 'Validate', Validate, ...
                 'Time', 1, 'Cap', 2, 'Type', 3);
    eval([name 'Tags = tags;']);
    eval(['sanity_check(' name 'Events, ' name 'Tags);']);
    clear Xevents c Commit CommitFA CommitPA EagerFA EagerPA cores Validate name tags
end

clear file files









