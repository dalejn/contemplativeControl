%% Script to apply optimal control

clear
clc

% set directory to main folder with scripts
cd('/Users/dalezhou/Box/contemplativeControlCode/daleManuscriptCodeReplication')

% Define numbers and subjects
nreg=414;
nedge=(nreg*(nreg-1))/2;

% Load fMRI task data

yeo_index = dlmread('data/schaefer400x7CommunityAffiliation.txt');
control_betas = dlmread('data/control_beta.txt', '\t', 1, 1);
mindfulness_betas = dlmread('data/mindfulness_beta.txt', '\t', 1, 1);
mindfulness_betas_alc_react = dlmread('data/mindfulness_beta_alc_react.txt', '\t', 1, 1);

fid = fopen('data/control_beta.txt', 'rt');
listcell = textscan(fid, '%s%*[^\n]', 'headerlines', 1);
fclose(fid);
control_subj_name = strrep(listcell{1}, '"', '');

fid = fopen('data/mindfulness_beta.txt', 'rt');
listcell = textscan(fid, '%s%*[^\n]', 'headerlines', 1);
fclose(fid);
mindfulness_subj_name =  strrep(listcell{1}, '"', '');

fid = fopen('data/task_beta_subcortical_order.txt', 'rt');
listcell = textscan(fid, '%s%*[^\n]', 'headerlines', 0);
fclose(fid);
task_subcortical_order =  strrep(listcell{1}, '"', '');

% Read in structural connectivity matrices
sc_network_files = dir('data/struc/*.mat');
nfiles = length(sc_network_files);
fa_sq = zeros(nfiles, nedge);
subj_name_vector = {};

for k=1:nfiles
    load(['data/struc/' sc_network_files(k).name])
    struc_subcortical_order = cellstr(schaefer414x7_region_labels);
    [~, idx_reorder_subcortical_nodes] = ismember(task_subcortical_order, {struc_subcortical_order{401:414}}');
    schaefer414x7_count_pass_connectivity(401:414);
    schaefer414x7_count_pass_connectivity2 = schaefer414x7_count_pass_connectivity([1:400 idx_reorder_subcortical_nodes'+400],[1:400 idx_reorder_subcortical_nodes'+400]);
    fa_sq(k,:) = squareform(schaefer414x7_count_pass_connectivity2);
    subj_name = split(sc_network_files(k).name, '-');
    subj_name = split(subj_name{2}, '_');
    subj_name = lower(subj_name{1})
    subj_name_vector{k, 1} = subj_name;
end

subj_name_vector = strrep(subj_name_vector, 'murip','muri')

% Prep input variables

nsub = size(subj_name_vector,1);
allBetas = zeros(nsub,nreg);
[~,idx_mindfulness] = ismember(mindfulness_subj_name, subj_name_vector);
[~,idx_control] = ismember(control_subj_name, subj_name_vector);
subjIDmindfulness = subj_name_vector(nonzeros(idx_mindfulness));
subjIDcontrol = subj_name_vector(nonzeros(idx_control));

% set parameters

nNodes = size(nreg, 1);
T = 3; % control horizon
nTimeSteps = 1000; % number of time steps in simulation
rho = 100; % parameter that weights energy and distance constraints, set as in Betzel et al, Sci. Rep (2016)
controlInputs = 'cognitiveControlRegions'; % parameter to set control inputs; possible values are 'allNodes', 'cognitiveControlRegions'

% loading parcel Yeo sub-network label for Lausanne-234 parcels
% 1=Visual, 2=Somatomator, 3=Dorsal Attention, 4=Ventral Attention, 5=Limbic, 6=Frontoparietal Control, 7=Default Mode, 8=Subcortical
subSystemLabels = {'Visual', 'Somatomator', 'DorsalAttention', 'VentralAttention', 'Limbic', 'FrontoparietalControl', 'DefaultMode', 'Subcortical'};
nSubSystems = numel(subSystemLabels);

% setting control inputs
switch controlInputs
    case 'allNodes'
        B = eye(size(schaefer414x7_count_pass_connectivity2)); % all nodes as control inputs
    case 'cognitiveControlRegions'
        B = diag(yeo_index==3 | yeo_index==4 | yeo_index==6); % dorsal and ventral attention networks + frontoparietal control network are control inputs
        B(end+14, end+14) = 0;
end


% compute optimal energy on controls for natural react

energy_cost_control = nan(size(control_betas,1), nreg);
logsum_energy_cost_control = nan(size(control_betas,1), nreg);
stability_control = nan(size(control_betas,1), nreg);
U_opt_stability_control = nan(nTimeSteps+1, nreg, size(control_betas,1));

for k=1:size(subjIDcontrol,1)
    currentSub = subjIDcontrol(k)
    [~,idx_struc] = ismember(currentSub, subj_name_vector);

    xf = control_betas(k,:)'; % final state = contrasts for task
    xf = xf/norm(xf); % IMPORTANT - normalizing brain states by Euclidean norm

    A = squareform(fa_sq(idx_struc,:)); % structural matrix
    
    % setting initial state to 0's
    x0 = zeros(size(xf));

    % setting diagonal elements of S to include only nodes within slab
    S = eye(nreg);

    % calculating optimal control
    [X_opt_x0_xf, U_opt_x0_xf, n_err] = opt_eng_cont(A, T, B, x0, xf, rho, S, true);

    energyCost = trapz(U_opt_x0_xf.^2)/nTimeSteps; % calculating the total energetic cost as the sum of integral of squared energy trajectories

    logsumEnergyCost = log10(sum(energyCost)); % last entry stores energy cost summed over all control inputs

    % calculating stability of brain state - control energy needed
    % to maintain state xf
    [X_opt_stability, U_opt_stability, ~] = opt_eng_cont(A, T, B, xf, xf, rho, S, true);
    X_opt_stability = X_opt_stability(:, 1:nNodes);
    energyCost_stability = trapz(U_opt_stability.^2)/nTimeSteps; % calculating the total energetic cost as the sum of integral of squared energy trajectories

    stability = 1./log10(sum(energyCost_stability)); % last entry stores stability summed over all control inputs
    
    energy_cost_control(k,:) = energyCost';
    logsum_energy_cost_control(k,:) = logsumEnergyCost; % storing total energy cost
    stability_control(k,:) = energyCost_stability'; % storing stability
    U_opt_stability_control(:, :, k) = U_opt_stability; % storing control input trajectories for stability calculation
end

% Figure 3A and 3D

% compute optimal control (mindfulness)
energy_cost_mindfulness= nan(size(mindfulness_betas,1), nreg);
logsum_energy_cost_mindfulness = nan(size(mindfulness_betas,1), nreg);
stability_mindfulness = nan(size(mindfulness_betas,1), nreg);
U_opt_stability_mindfulness = nan(nTimeSteps+1, nreg, size(mindfulness_betas,1));

for k=1:size(subjIDmindfulness,1)
    currentSub = subjIDmindfulness(k)
    [~,idx_struc] = ismember(currentSub, subj_name_vector);
    
    xf = mindfulness_betas(k,:)'; % final state = contrasts for task
    xf = xf/norm(xf); % IMPORTANT - normalizing brain states by Euclidean norm

    A = squareform(fa_sq(idx_struc,:)); % structural matrix

    % setting initial state to 0's
    x0 = zeros(size(xf));

    % setting diagonal elements of S to include only nodes within slab
    S = eye(nreg);

    % calculating optimal control
    [X_opt_x0_xf, U_opt_x0_xf, n_err] = opt_eng_cont(A, T, B, x0, xf, rho, S, true);

    energyCost = trapz(U_opt_x0_xf.^2)/nTimeSteps; % calculating the total energetic cost as the sum of integral of squared energy trajectories

    logsumEnergyCost = log10(sum(energyCost)); % last entry stores energy cost summed over all control inputs

    % calculating stability of brain state - control energy needed
    % to maintain state xf
    [X_opt_stability, U_opt_stability, ~] = opt_eng_cont(A, T, B, xf, xf, rho, S, true);
    X_opt_stability = X_opt_stability(:, 1:nNodes);
    energyCost_stability = trapz(U_opt_stability.^2)/nTimeSteps; % calculating the total energetic cost as the sum of integral of squared energy trajectories

    stability = 1/log10(sum(energyCost_stability)); % last entry stores stability summed over all control inputs
    
    energy_cost_mindfulness(k,:) = energyCost';
    logsum_energy_cost_mindfulness(k,:) = logsumEnergyCost; % storing total energy cost
    stability_mindfulness(k,:) = energyCost_stability'; % storing stability
    U_opt_stability_mindfulness(:, :, k) = U_opt_stability; % storing control input trajectories for stability calculation
end

% compute stability (mindfulness manipulation)
energy_cost_alc_react_mindfulness = nan(size(mindfulness_betas_alc_react,1), nreg);
logsum_energy_cost_alc_react_mindfulness = nan(size(mindfulness_betas_alc_react,1), nreg);
stability_alc_react_mindfulness = nan(size(mindfulness_betas_alc_react,1), nreg);
U_opt_stability_alc_react_mindfulness = nan(nTimeSteps+1, nreg, size(mindfulness_betas_alc_react,1));

for k=1:size(subjIDmindfulness,1)
    currentSub = subjIDmindfulness(k)
    [~,idx_struc] = ismember(currentSub, subj_name_vector);
    
    x0 = mindfulness_betas_alc_react(k,:)'; % final state = contrasts for task
    x0 = x0/norm(x0); % IMPORTANT - normalizing brain states by Euclidean norm

    A = squareform(fa_sq(idx_struc,:)); % structural matrix

    % setting initial state to 0's
    xf = mindfulness_betas(k,:)'; % final state = contrasts for task
    xf = xf/norm(xf); % IMPORTANT - normalizing brain states by Euclidean norm
    
    % setting diagonal elements of S to include only nodes within slab
    S = eye(nreg);

    % calculating optimal control
    [X_opt_x0_xf, U_opt_x0_xf, n_err] = opt_eng_cont(A, T, B, x0, xf, rho, S, true);

    energyCost = trapz(U_opt_x0_xf.^2)/nTimeSteps; % calculating the total energetic cost as the sum of integral of squared energy trajectories

    logsumEnergyCost = log10(sum(energyCost)); % last entry stores energy cost summed over all control inputs

    % calculating stability of brain state - control energy needed
    % to maintain state xf
    [X_opt_stability, U_opt_stability, ~] = opt_eng_cont(A, T, B, xf, xf, rho, S, true);
    X_opt_stability = X_opt_stability(:, 1:nNodes);
    energyCost_stability = trapz(U_opt_stability.^2)/nTimeSteps; % calculating the total energetic cost as the sum of integral of squared energy trajectories

    stability = 1/log10(sum(energyCost_stability)); % last entry stores stability summed over all control inputs
    
    energy_cost_alc_react_mindfulness(k,:) = energyCost';
    logsum_energy_cost_alc_react_mindfulness(k,:) = logsumEnergyCost; % storing total energy cost
    stability_alc_react_mindfulness(k,:) = energyCost_stability'; % storing stability
    U_opt_stability_alc_react_mindfulness(:, :, k) = U_opt_stability; % storing control input trajectories for stability calculation
end

% Figure 3C and 3E

% compute optimal control and stability (mindfulness natural react)
energy_cost_mindfulness_natural_react= nan(size(mindfulness_betas_alc_react,1), nreg);
logsum_energy_cost_mindfulness_natural_react = nan(size(mindfulness_betas_alc_react,1), nreg);
stability_mindfulness_natural_react = nan(size(mindfulness_betas_alc_react,1), nreg);
U_opt_stability_mindfulness_natural_react = nan(nTimeSteps+1, nreg, size(mindfulness_betas_alc_react,1));

for k=1:size(subjIDmindfulness,1)
    currentSub = subjIDmindfulness(k)
    [~,idx_struc] = ismember(currentSub, subj_name_vector);
    
    xf = mindfulness_betas_alc_react(k,:)'; % final state = contrasts for task
    xf = xf/norm(xf); % IMPORTANT - normalizing brain states by Euclidean norm

    A = squareform(fa_sq(idx_struc,:)); % structural matrix

    % setting initial state to 0's
    x0 = zeros(size(xf));

    % setting diagonal elements of S to include only nodes within slab
    S = eye(nreg);

    % calculating optimal control
    [X_opt_x0_xf, U_opt_x0_xf, n_err] = opt_eng_cont(A, T, B, x0, xf, rho, S, true);

    energyCost = trapz(U_opt_x0_xf.^2)/nTimeSteps; % calculating the total energetic cost as the sum of integral of squared energy trajectories

    logsumEnergyCost = log10(sum(energyCost)); % last entry stores energy cost summed over all control inputs

    % calculating stability of brain state - control energy needed
    % to maintain state xf
    [X_opt_stability, U_opt_stability, ~] = opt_eng_cont(A, T, B, xf, xf, rho, S, true);
    X_opt_stability = X_opt_stability(:, 1:nNodes);
    energyCost_stability = trapz(U_opt_stability.^2)/nTimeSteps; % calculating the total energetic cost as the sum of integral of squared energy trajectories

    stability = 1/log10(sum(energyCost_stability)); % last entry stores stability summed over all control inputs
    
    energy_cost_mindfulness_natural_react(k,:) = energyCost';
    logsum_energy_cost_mindfulness_natural_react(k,:) = logsumEnergyCost; % storing total energy cost
    stability_mindfulness_natural_react(k,:) = energyCost_stability'; % storing stability
    U_opt_stability_mindfulness_natural_react(:, :, k) = U_opt_stability; % storing control input trajectories for stability calculation
end

%compute stability (mindfulness natural react)
energy_cost_mindfulness_alc_react = nan(size(mindfulness_betas_alc_react,1), nreg);
logsum_energy_cost_mindfulness_alc_react = nan(size(mindfulness_betas_alc_react,1), nreg);
stability_mindfulness_alc_react = nan(size(mindfulness_betas_alc_react,1), nreg);
U_opt_stability_mindfulness_alc_react = nan(nTimeSteps+1, nreg, size(mindfulness_betas_alc_react,1));

for k=1:size(subjIDmindfulness,1)
    currentSub = subjIDmindfulness(k)
    [~,idx_struc] = ismember(currentSub, subj_name_vector);
    
    xf = mindfulness_betas_alc_react(k,:)'; % final state = contrasts for task
    xf = xf/norm(xf); % IMPORTANT - normalizing brain states by Euclidean norm

    A = squareform(fa_sq(idx_struc,:)); % structural matrix

    % setting initial state to 0's
    x0 = mindfulness_betas(k,:)'; % final state = contrasts for task
    x0 = x0/norm(x0); % IMPORTANT - normalizing brain states by Euclidean norm
    
    % setting diagonal elements of S to include only nodes within slab
    S = eye(nreg);

    % calculating optimal control
    [X_opt_x0_xf, U_opt_x0_xf, n_err] = opt_eng_cont(A, T, B, x0, xf, rho, S, true);

    energyCost = trapz(U_opt_x0_xf.^2)/nTimeSteps; % calculating the total energetic cost as the sum of integral of squared energy trajectories

    logsumEnergyCost = log10(sum(energyCost)); % last entry stores energy cost summed over all control inputs

    % calculating stability of brain state - control energy needed
    % to maintain state xf
    [X_opt_stability, U_opt_stability, ~] = opt_eng_cont(A, T, B, xf, xf, rho, S, true);
    X_opt_stability = X_opt_stability(:, 1:nNodes);
    energyCost_stability = trapz(U_opt_stability.^2)/nTimeSteps; % calculating the total energetic cost as the sum of integral of squared energy trajectories

    stability = 1/log10(sum(energyCost_stability)); % last entry stores stability summed over all control inputs
    
    energy_cost_mindfulness_alc_react(k,:) = energyCost';
    logsum_energy_cost_mindfulness_alc_react(k,:) = logsumEnergyCost; % storing total energy cost
    stability_mindfulness_alc_react(k,:) = energyCost_stability'; % storing stability
    U_opt_stability_mindfulness_alc_react(:, :, k) = U_opt_stability; % storing control input trajectories for stability calculation
end

% Save results for plotting

dlmwrite('results/stability_control_react.txt', nanmean(stability_control(:,(yeo_index==3 | yeo_index==4 | yeo_index==6)),1), ' ')
dlmwrite('results/stability_mindfulness_downreg.txt', nanmean(stability_alc_react_mindfulness(:,(yeo_index==3 | yeo_index==4 | yeo_index==6)),1), ' ')
dlmwrite('results/stability_mindfulness_alc_react.txt', nanmean(stability_mindfulness_alc_react(:,(yeo_index==3 | yeo_index==4 | yeo_index==6)),1), ' ')

dlmwrite('results/energy_cost_zero_to_control.txt', nanmean(energy_cost_control(:,(yeo_index==3 | yeo_index==4 | yeo_index==6)),1), ' ')
dlmwrite('results/energy_cost_zero_to_mindfulness.txt', nanmean(energy_cost_mindfulness(:,(yeo_index==3 | yeo_index==4 | yeo_index==6)),1), ' ')
dlmwrite('results/energy_cost_zero_to_mindfulness_natural_react.txt', nanmean(energy_cost_mindfulness_natural_react(:,(yeo_index==3 | yeo_index==4 | yeo_index==6)),1), ' ') 

writecell(subjIDcontrol, 'results/control_subj_ID.txt')
writecell(subjIDmindfulness, 'results/mindfulness_subj_ID.txt')