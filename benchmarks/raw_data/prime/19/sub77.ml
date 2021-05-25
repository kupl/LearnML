let prime : int -> bool
= fun n -> (*TODO*)

#include <bits/stdc++.h> 
using namespace std; 
  
// function check whether a number  
// is prime or not 
bool isPrime(int n) 
{ 
    // Corner case 
    if (n <= 1) 
        return false; 
  
    // Check from 2 to n-1 
    for (int i = 2; i < n; i++) 
        if (n % i == 0) 
            return false; 
  
    return true; 
} 
  
// Driver Program 
int main() 
{ 
    isPrime(11) ? cout << " true\n" :  
                  cout << " false\n"; 
    return 0; 
} 