import numpy as np
from scenario_tree import *
   
class EasyStochasticModel(StochModel):
    def __init__(self, sim_setting):
        self.averages = sim_setting['averages']
        self.dim_obs = len(sim_setting['averages'])

    def simulate_one_time_step(self, parent_node, n_children):
        probs = np.ones(n_children)/n_children
        obs = np.random.multivariate_normal(
            mean=self.averages,
            cov=np.identity(self.dim_obs),
            size=n_children
        ).T # obs.shape = (len_vector, n_children)
        return probs, obs 

sim_setting = {
    'averages': [0.4,0.3,0.2,0.1]
}
easy_model = EasyStochasticModel(sim_setting)
scen_tree = ScenarioTree(
    name="test_newsvendor",
    branching_factors=[100],
    len_vector=4,
    initial_value=[0,1,2,3],
    stoch_model=easy_model,
)

scen_tree.plot()

# Riduzione degli scenari finali a 10 cluster
reduced_scenarios = scen_tree.reduce_with_clustering(num_clusters=10)

# Visualizza i risultati
print("Scenari ridotti di test tree:")
print(reduced_scenarios)