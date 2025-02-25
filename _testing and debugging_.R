#____Testing, debugging____

#load planetscope data
ps_files = list.files('data/PSScene', full.names = T)
ps_rast_files = ps_files[str_detect(ps_files, '\\.tif$')&str_detect(ps_files, '_SR_')]
ps = rast(ps_rast_files[1])

#load landsat data
land_files = list.files('data/landsat', full.names = T)
land_list = landstack('data/landsat')
land = land_list[[1]]

#crop landsat to match planetscope and vice versa
land_C = crop(land, ps)
ps_c = crop(ps, land)




{
print('8 threads')
tic()
resample(x = ps, y = land, method = 'average', threads = 8)
toc()

print('1 thread')
tic()
resample(x = ps, y = land, method = 'average', threads = 1)
toc()

print('threads param not defined')
tic()
resample(x = ps, y = land, method = 'average')
toc()}


library(ClusterR)
a = values(ps_c)

tic()
kmeans(a, centers = 10)
toc()