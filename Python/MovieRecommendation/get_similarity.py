import pickle
with open("./data/movie_feature_matrix.txt", "rb") as fp:   # Unpickling
    movie_feature_matrix = pickle.load(fp)

#===========================

import sys
try:
  i_id = int(sys.argv[1])
except:
  i_id = 1

#===========================

from sklearn.metrics.pairwise import cosine_similarity

i_vec = [item for item in movie_feature_matrix if item[0] == i_id]
i_vec = i_vec[0][1]

m_vec=list()
for i in movie_feature_matrix:
  m_vec.append(i[1])

#===========================

cos_sim = cosine_similarity(m_vec,[i_vec])

sim = list()
for i in range(len(cos_sim)):
  sim.append((movie_feature_matrix[i][0],cos_sim[i][0]))

sim.sort(key=lambda tup: tup[1], reverse=True)

#===========================

import json
print(json.dumps(sim[1:21]))
