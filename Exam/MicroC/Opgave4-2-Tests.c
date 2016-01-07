void main() {
    print 2 .< 3 .< 4; 
    print 3 .< 2 .== 2; 
    print 3 .> 2 .== 2; 
    print (3 .> 2 .== 2) == (3 .> 1 .== 1); 
    print (3 .> 2 .== 2) == 1; 
    // prints 1 0 1 1 1
    
    println; // True true
    print -1 .< 2 .> -2;  
    print 1 .== 1 .!= 2;
    print 1+4 .>= 5-1 .<= 100-10;
    print (1 .== 1 .== 1) .== 1 .< 3;
    // prints 1 1 1
    
    println; // false false
    print -1 .> 2 .< -2;  
    print 1 .!= 1 .== 2;
    print 1+4 .<= 5-1 .>= 100-10; 
    // prints 0 0 0
    
    println; // true false
    print -1 .< 2 .< -2;  
    print 1 .== 1 .== 2;
    print 1+4 .>= 5-1 .>= 100-10; 
    // prints 0 0 0
    
    println; // false true
    print -1 .> 2 .> -2;  
    print 1 .!= 1 .!= 2;
    print 1+4 .<= 5-1 .<= 100-10; 
    // prints 0 0 0
    
    int x;
    x = 5;
    println; // True true
    print x .== x .== x;  
    print x .== 5 .== x;
    print 5 .== x .== 5;
    // prints 1 1 1
}