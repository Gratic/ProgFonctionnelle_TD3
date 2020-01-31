package recfun

object Main {

   def main(args: Array[String]) {
        println("Pascal's Triangle")
        for (row <- 0 to 10) {
            for (col <- 0 to row)
                print(coefBinomial(col, row) + " ")
            println()
        }
    }

    /**
     * Exercise 1
     * Triangle de Pascal
     */
    /**def coefBinomial(col: Int, ligne: Int): Int = {
      def factoriel(n : Int) : Int = {
        n match {
          case 0 => 1
          case 1 => 1
          case _ => n * factoriel(n-1)
        }
      }
      
      if (col > ligne || col < 0)
        throw new IllegalArgumentException
      else
        factoriel(ligne)/(factoriel(col)*factoriel(ligne-col))
    }*/
   
   def coefBinomial(col: Int, ligne: Int): Int = {
     if (col > ligne) throw new IllegalArgumentException
     if (col < 0) throw new IllegalArgumentException
     if (ligne < 0) throw new IllegalArgumentException
     
     (col, ligne) match {
       case (0,_) => 1
       case (col, ligne) if col == ligne => 1
       case _ => coefBinomial(col-1, ligne-1) + coefBinomial(col, ligne-1)
     }
   }
   

    /**
     * Exercise 2
     * Écrire une fonction récursive qui renvoie vraie ssi une liste de caractères est un palindrome
     */
    def isPalindrome(list: List[Char]): Boolean = {
      def renverse(myList: List[Any]): List[Any] = 
      {
        myList match {
          case Nil => throw new IllegalArgumentException
          case first::Nil => List(first)
          case _ => myList.last::renverse(myList.init)
        }
      }
      
      list match {
        case Nil => true
        case list if list == renverse(list) => true
        case _ => false
      }
    }

    /**
     * Exercise 3
     * Soit, d'après B.Werber, la suite débutant par 1 et telle que chaque terme soit l'énumération du terme  précédent,  énumération  constituée  simplement  par
     * le  nombre  de  fois  qu'un  chiffre  se trouve plusieurs fois écrit consécutivement. Ainsi à l'étape 111221, la lecture de gauche à droite de ce "mot" sera
     * 3 fois le chiffre 1 puis 2fois le chiffre 2 et enfin 1 fois le chiffre 1. L'étape suivante sera donc constituée par 312211.
     */
    /**
    def ant(etape: List[Int], nombreIteration: Int): List[Int] = {
      def iterate(myList: List[Int], count: Int, number: Int): List[Int] = {
       (myList,count,number) match {
         case (Nil,_,_) => Nil
         case (head::tail,_,number) if head != number => count::number::iterate(tail,0,tail.head)
         case (_,_,_) if count > etape.count => Nil
         case _ => iterate(myList.tail, count, number)
       }
     }
      
     (etape, nombreIteration) match {
       case (Nil,_) => throw new IllegalArgumentException
       case (_, 0) => etape
       case _ => ant(iterate(etape.tail, 0, etape.head), nombreIteration - 1) 
     }
    }
    */
    def ant(etape: List[Int], nombreIteration: Int): List[Int] = {
      def iterate(etape: List[Int], number: Int, count: Int): List[Int] = {
        etape match {
          case Nil => List(count,number)
          case elem::reste if elem != number => count::number::iterate(reste, elem, 1)
          case elem::reste => iterate(reste, number, count + 1)
        }
      }
      
      (etape, nombreIteration) match {
       case (Nil,_) => throw new IllegalArgumentException
       case (_, 0) => etape
       case _ => ant(iterate(etape.tail, etape.head, 1), nombreIteration - 1) 
     }
    }
    

    /**
     * Exercise 4
     * Équilibrage des parenthèses dans une chaîne de caractères.
     */
    /**def equili(chars: List[Char]): Boolean = {
       def aux(chars: List[Char], state: Int, impossible: Boolean) : Boolean = {
         (chars, state, impossible) match {
           case (chars,_,_) if chars.isEmpty && state != 0 => false
           case (chars,_,_) if chars.isEmpty && state == 0 => true
           case (_,_,impossible) if impossible => false
           case (_,state,_) if state < 0 => aux(chars, state, true)
           case (c::reste,_,_) if c == '(' => aux(reste, state + 1, impossible)
           case (c::reste,_,_) if c == ')' => aux(reste, state - 1, impossible)
           case _ => aux(chars.tail, state, impossible)
         }
       }
       
       aux(chars, 0, false)
    }*/
    
    def equili(chars: List[Char]): Boolean = {
       def aux(chars: List[Char], state: Int) : Boolean = {
         (chars, state) match {
           case (chars,_) if chars.isEmpty && state != 0 => false
           case (chars,_) if chars.isEmpty && state == 0 => true
           case (_,state) if state < 0 => false
           case (c::reste,_) if c == '(' => aux(reste, state + 1)
           case (c::reste,_) if c == ')' => aux(reste, state - 1)
           case _ => aux(chars.tail, state)
         }
       }
       
       aux(chars, 0)
    }

    /**
     * Exercise 5
     * Écrire une fonction booléenne et récursive qui teste si une chaîne de caractères donnée  est  une  anagramme  d'une
     * autre  chaîne  de  caractères  donnée.  Par  exemple : 'algorithme' est une anagramme de 'logarithme'
     */
    def isAnagramme(chars1: List[Char], chars2: List[Char]): Boolean = {
      def delFirst(chars :List[Char], char : Char) : List[Char] = {
        chars match {
          case Nil => Nil
          case elem::reste if elem == char => reste
          case elem::reste => elem::delFirst(reste, char)
        }
      }
      
      (chars1, chars2) match {
        case (chars1, chars2) if chars1.size != chars2.size => false
        case (Nil, Nil) => true
        case (chars1, chars2) => isAnagramme(
            delFirst(chars1.map(c => c.toUpper), chars1(0).toUpper),
            delFirst(chars2.map(c => c.toUpper), chars1(0).toUpper)
            )
      }
    }

    /**
     * Exercise 6
     * Écrire une fonction récursive qui calcule le pgcd de deux entiers naturels a et b par la méthode d'Euclide
     */
    def pgcd(a: Int, b: Int): Int = {
      def euclide(a: Int, b: Int): Int = {
        b match {
          case 0 => a
          case _ => euclide(b,a%b)
        }
      }
      euclide(a,b)
    }

    /**
     * Exercise 7
     * Écrire une fonction récursive change qui détermine le nombre de façon différentes de payer une somme d’argents a l’aide
     * d’une liste de valeurs de billets disponibles. Nous supposerons que cette liste est donnée de façon décroissante.
     * Par exemple : change (List(5, 10, 20, 50, 100, 200, 500),1790 ) = 1602321
     */
    def nombreDeChange(monnaies: List[Int], montant: Int): Int = ???



}
