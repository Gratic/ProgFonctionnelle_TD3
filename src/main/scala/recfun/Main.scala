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
    def coefBinomial(col: Int, ligne: Int): Int = {
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
    def ant(etape: List[Int], nombreIteration: Int): List[Int] = {
       def iterate(etape : List[Int]) : List[Int] = {
         
       }
      
       (etape, nombreIteration) match {
         case (Nil,_) => throw new IllegalArgumentException
         case (_, 0) => etape
         case (_, nombreIteration) => ant(machin(etape),nombreIteration - 1) 
       }
       Nil
    }

    /**
     * Exercise 4
     * Équilibrage des parenthèses dans une chaîne de caractères.
     */
    def equili(chars: List[Char]): Boolean = ???

    /**
     * Exercise 5
     * Écrire une fonction booléenne et récursive qui teste si une chaîne de caractères donnée  est  une  anagramme  d'une
     * autre  chaîne  de  caractères  donnée.  Par  exemple : 'algorithme' est une anagramme de 'logarithme'
     */
    def isAnagramme(chars1: List[Char], chars2: List[Char]): Boolean = ???

    /**
     * Exercise 6
     * Écrire une fonction récursive qui calcule le pgcd de deux entiers naturels a et b par la méthode d'Euclide
     */
    def pgcd(a: Int, b: Int): Int = ???

    /**
     * Exercise 7
     * Écrire une fonction récursive change qui détermine le nombre de façon différentes de payer une somme d’argents a l’aide
     * d’une liste de valeurs de billets disponibles. Nous supposerons que cette liste est donnée de façon décroissante.
     * Par exemple : change (List(5, 10, 20, 50, 100, 200, 500),1790 ) = 1602321
     */
    def nombreDeChange(monnaies: List[Int], montant: Int): Int = ???



}
