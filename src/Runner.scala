object Runner {
    def main(args: Array[String]) {
		if (args.length != 3 ) printUsage
		else{
			val command = args(0)
			command match {
				case "-e" => println(encrypt(args(1),args(2)))
				case "-d" => println(decrypt(args(1),args(2)))
				case _ => printUsage
			}
		}
		
	}
	private def printUsage {
		println("Usage: main -e <srouce string> <cipher> //Encrypt source string with the cipher by vigenere algorithm;")
		println("       main -d <encrypted string> <cipher> //Decrypt encrypted string with the cipher by vigenere algorithm;")
	}
	private def encrypt(srcStr: String, cipher: String): String = encryptInternal(srcStr.toList, cipher.toList, cipher.toList).mkString
	
	private def encryptInternal(srcStr: List[Char], cipherRemain: List[Char], cipher: List[Char]): List[Char] = {
		srcStr match {
			case Nil => Nil
			case sc::ss if isLetter(sc) => 
				cipherRemain match {
					case Nil => encryptInternal(srcStr, cipher, cipher)
					case cc::cs => encryptChar(sc.toUpper, cc.toUpper)::encryptInternal(ss, cs, cipher)
				}
			case sc::ss => ' '::encryptInternal(ss, cipherRemain, cipher)
		}			
	}
	
	private def encryptChar(srcChar: Char, cipherChar: Char): Char = ((((srcChar - 'A') + (cipherChar - 'A')) % 26) + 'A').toChar
	
	private def decrypt(encStr: String, cipher: String): String = decryptInternal(encStr.toList, cipher.toList, cipher.toList).mkString
	
	private def decryptInternal(encStr: List[Char], cipherRemain: List[Char], cipher: List[Char]): List[Char] = {
		encStr.toList match {
			case Nil => Nil
			case ec::es if isLetter(ec) => 
				cipherRemain match {
					case Nil => decryptInternal(encStr, cipher, cipher)
					case cc::cs => decryptChar(ec.toUpper, cc.toUpper)::decryptInternal(es, cs, cipher)
				}
			case ec::es => ' '::decryptInternal(es, cipherRemain, cipher)
		}			
	}
	
	private def decryptChar(encChar: Char, cipherChar: Char): Char = ((((encChar + 26) - cipherChar) % 26) + 'A').toChar

  private def isLetter(c: Char): Boolean = if (c>='A' && c<='Z') true else false
}