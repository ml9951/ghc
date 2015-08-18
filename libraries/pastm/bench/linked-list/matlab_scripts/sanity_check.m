function [] = sanity_check(events, tags)
    
for i = 1:length(events)
    e = events{i};
    
    startTX = true;  %looking for the start of a transaction
    for j = 1:size(e, 1)
        if startTX
            if e(j, 2) <= 0 %Start TX Operations have positive tags
                error(['Expceted positive Start TX tag, but instead got ' num2str(e(j, 2))]); 
            else
                startTX = false;
                continue;
            end
        end
        if ~startTX && e(j, 2) == tags.Commit
            startTX = true;
        end
    end
end

disp('Data is OK')
