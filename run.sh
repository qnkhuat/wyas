ghc Main.hs
if [ $? -eq 0 ]
then 
    ./Main "(?eq + +)"
    ./Main "(a '(quoted (dotted . list)) test)"
    ./Main "(?eq + + -)"
    ./Main "(- (+ 4 6 3) 3 5 2)"
fi
