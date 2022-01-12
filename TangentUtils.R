#Tangent Utils

#returns data frame with a and b where a is the y intercept and b the slope
get_tangent = function(spline, xPos){
  pred0 = predict(spline, x=xPos, deriv=0)
  pred1 = predict(spline, x=xPos, deriv=1)
  
  b = pred1$y 
  a = pred0$y - b*xPos
  return(data.frame(a,b))
}

find_next_point_tangent = function(spline,xPos, slope , direction = 1, step_size = 0.1,accuracy = 0.1, max_dist = 10){
  currPos = xPos
  while(1){
    if(abs(xPos-currPos) > max_dist){
      print("Error max dist elapsed with no tangent")
      return(NaN)
    }
    currPos = currPos + step_size 
    currTan = get_tangent(spline, currPos)
    if(currTan$b > slope-accuracy && currTan$b < slope+accuracy){
      return(currTan)
    } 
  }
   
}


demo = function(x,y, tanPoint){
  #create smooth approximation to points
  spline = smooth.spline(x, y, spar=0.35)
  plot(x,y)
  lines(spline, col = "lightgray")
 
  tan = get_tangent(spline, tanPoint)
  abline(a = tan$a, b = tan$b) 
  
  newTan = find_next_point_tangent(spline, tanPoint, tan$b)
  abline(a = newTan$a, b =  newTan$b, col = "lightblue")
  
  
}























