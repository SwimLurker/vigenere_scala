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
	private def encrypt(srcStr: String, cipher: String): String = encryptInternal(srcStr, cipher, cipher)
	
	private def encryptInternal(srcStr: String, cipherRemain: String, cipher: String): String = {
		srcStr match {
			case Nil => Nil
			case sc::ss if isLetter(sc) => 
				cipherRemain match {
					case Nil = > encryptInternal(srcStr, cipher, cipher)
					case cc::cs => encryptChar(toUpper(sc), toUpper(cc))::encryptInternal(ss, cs, cipher)
				}
			case _ => ' '::encryptInternal(ss, cipherRemain, cipher)
		}			
	}
	
	private def encryptChar(srcChar: Char, cipherChar: Char): Char = (((srcChar - 'A') + (cipherChar - 'A')) mod 26) + 'A'
	
	private def decrypt(encStr: String, cipher: String): String = decryptInternal(encStr, cipher, cipher)
	
	private def decryptInternal(encStr: String, cipherRemain: String, cipher: String): String = {
		encStr match {
			case Nil => Nil
			case ec::es if isLetter(ec) => 
				cipherRemain match {
					case Nil = > decryptInternal(encStr, cipher, cipher)
					case cc::cs => decryptChar(toUpper(ec), toUpper(cc))::decryptInternal(es, cs, cipher)
				}
			case _ => ' '::decryptInternal(es, cipherRemain, cipher)
		}			
	}
	
	private def decryptChar(encChar: Char, cipherChar: Char): Char = (((encChar + 26) - cipherChar) mod 26) + 'A'
	
}