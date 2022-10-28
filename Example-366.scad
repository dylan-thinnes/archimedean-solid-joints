// Find the unitary vector with direction v. Fails if v=[0,0,0].           
function unit(v) = norm(v)>0 ? v/norm(v) : undef;                          
// Find the transpose of a rectangular matrix                              
function transpose(m) = // m is any rectangular matrix of objects          
  [ for(j=[0:len(m[0])-1]) [ for(i=[0:len(m)-1]) m[i][j] ] ];              
// The identity matrix with dimension n                                    
function identity(n) = [for(i=[0:n-1]) [for(j=[0:n-1]) i==j ? 1 : 0] ];    
                                                                           
// computes the rotation with minimum angle that brings a to b             
// the code fails if a and b are opposed to each other                     
function rotate_from_to(a,b) =                                             
    let( axis = unit(cross(a,b)) )                                         
    axis*axis >= 0.99 ?                                                    
        transpose([unit(b), axis, cross(axis, unit(b))]) *                 
            [unit(a), axis, cross(axis, unit(a))] :                        
        identity(3);                                                       
                                                                           
module line(p0, p1, diameter=1) {                                          
    v = p1-p0;                                                             
    translate(p0)                                                          
        // rotate the cylinder so its z axis is brought to direction v     
        multmatrix(rotate_from_to([0,0,1],v))                              
            cylinder(d=diameter, h=norm(v), $fn=16);                       
}                                                                          
scale(60) difference() {
 union() {
  difference() {
   sphere(0.2, $fn=16);
   translate([0,0,-1]) cube(2, center=true, $fn=16);
  }
  translate([0,0,-0.05]) cylinder(0.05, 0.2, 0.2, $fn=16);
 }
 line([-0.26111648393354714,-0.8660254037844386,0.4264014327112208], [0.0,0.0,-0.0], diameter=0.10166666666666666);
 line([-0.26111648393354714,0.8660254037844386,0.4264014327112208], [0.0,0.0,-0.0], diameter=0.10166666666666666);
 line([0.6963106238227909,0.5773502691896257,0.4264014327112208], [0.0,0.0,-0.0], diameter=0.10166666666666666);
}

