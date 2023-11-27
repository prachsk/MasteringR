# Taks for W1 D2
point1D <- setClass('Point1D', slots = list(x='numeric'))
point2D <- setClass('Point2D', slots = list(x='numeric', y='numeric'))
point3D <- setClass('Point3D', slots = list(x='numeric', y='numeric', z='numeric'))

setGeneric('point', function(point1, point2) {
  standardGeneric('point')
})

setMethod('point', signature = c(point1 = 'Point1D', point2 = 'Point1D'),
          function(point1, point2){
            return(sqrt((point1@x - point2@x)^2))
          })
setMethod('point', signature = c(point1 = 'Point2D', point2 = 'Point2D'),
          function(point1, point2){
            return(sqrt((point1@x - point2@x)^2 + (point1@y - point2@y)^2))
          })
setMethod('point', signature = c(point1 = 'Point3D', point2 = 'Point3D'),
          function(point1, point2){
            return(sqrt((point1@x - point2@x)^2 + (point1@y - point2@y)^2 + (point1@z - point2@z)^2))
          })

my_1D <- vector('list', 10)
my_2D <- vector('list', 10)
my_3D <- vector('list', 10)

my_1D <- lapply(my_1D, function(x){
  point1D(x=sample(1:10,1))
})

my_2D <- lapply(my_2D, function(x){
  point2D(x=sample(1:10,1), y=sample(1:10,1))
})

my_3D <- lapply(my_3D, function(x){
  point3D(x=sample(1:10,1), y=sample(1:10,1), z=sample(1:10,1))
})

point_distance <- function(origin, end.points){
  distance <- c()
  
  for(i in 1:length(end.points)){
    distance <- c(distance, point(origin, end.points[[i]]))
  }
  
  return(distance)
}

distance_1D <- point_distance(point1D(x=0), my_1D)
distance_2D <- point_distance(point2D(x=0, y=0), my_2D)
distance_3D <- point_distance(point3D(x=0, y=0, z=0), my_3D)
