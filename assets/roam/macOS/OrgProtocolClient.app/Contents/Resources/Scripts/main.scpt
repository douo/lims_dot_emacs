FasdUAS 1.101.10   ��   ��    k             l     ��  ��      macOS 14.2     � 	 	    m a c O S   1 4 . 2   
  
 l     ��  ��    5 / ?????? "org-protocol://capture://href/title/"      �   ^  Nw�SN:N�NH   " o r g - p r o t o c o l : / / c a p t u r e : / / h r e f / t i t l e / "        l     ��  ��    2 , ???? "org-protocol://capture//href/title/"      �   X  O���lb   " o r g - p r o t o c o l : / / c a p t u r e / / h r e f / t i t l e / "        l     ��  ��      ????? : ??????????????     �   .  \N�{,N�N*   :  ���N*Q�epu(N�V�Y{,N�N*Q�S�      i         I      �� ���� 0 
converturl 
convertURL   ��  o      ���� 0 inputurl inputURL��  ��    k     8      !   l     �� " #��   " * $ Find the position of the first "//"    # � $ $ H   F i n d   t h e   p o s i t i o n   o f   t h e   f i r s t   " / / " !  % & % l     ' ( ) ' r      * + * m      , , � - -  / / + n      . / . 1    ��
�� 
txdl / 1    ��
�� 
ascr (   Split the URL by "/"    ) � 0 0 *   S p l i t   t h e   U R L   b y   " / " &  1 2 1 l   �� 3 4��   3 @ : Split and check if there's a double-slash without a colon    4 � 5 5 t   S p l i t   a n d   c h e c k   i f   t h e r e ' s   a   d o u b l e - s l a s h   w i t h o u t   a   c o l o n 2  6 7 6 Z    5 8 9�� : 8 H     ; ; D     < = < l   
 >���� > n    
 ? @ ? 4    
�� A
�� 
citm A m    	����  @ o    ���� 0 inputurl inputURL��  ��   = m   
  B B � C C  : 9 k    / D D  E F E l   �� G H��   G 6 0 Reconstruct with a quote after the double-slash    H � I I `   R e c o n s t r u c t   w i t h   a   q u o t e   a f t e r   t h e   d o u b l e - s l a s h F  J�� J r    / K L K c    - M N M b    + O P O b     Q R Q l    S���� S c     T U T l    V���� V n     W X W 7   �� Y Z
�� 
citm Y m    ����  Z m    ����  X o    ���� 0 inputurl inputURL��  ��   U m    ��
�� 
ctxt��  ��   R m     [ [ � \ \  : / / P l   * ]���� ] n    * ^ _ ^ 7    *�� ` a
�� 
citm ` m   $ &����  a m   ' )������ _ o     ���� 0 inputurl inputURL��  ��   N m   + ,��
�� 
ctxt L o      ���� 0 convertedurl convertedURL��  ��   : l  2 5 b c d b r   2 5 e f e o   2 3���� 0 inputurl inputURL f o      ���� 0 convertedurl convertedURL c   No change needed    d � g g "   N o   c h a n g e   n e e d e d 7  h i h l  6 6��������  ��  ��   i  j�� j L   6 8 k k o   6 7���� 0 convertedurl convertedURL��     l m l l     ��������  ��  ��   m  n o n i     p q p I     �� r��
�� .GURLGURLnull��� ��� TEXT r o      ���� 0 this_url this_URL��   q k     � s s  t u t l     �� v w��   v / ) display dialog "The URL is: " & this_URL    w � x x R   d i s p l a y   d i a l o g   " T h e   U R L   i s :   "   &   t h i s _ U R L u  y z y r      { | { m      } } � ~ ~ 4 / u s r / l o c a l / b i n / e m a c s c l i e n t | o      ���� $0 emacsclientpath1 emacsclientPath1 z   �  r     � � � m     � � � � � : / o p t / h o m e b r e w / b i n / e m a c s c l i e n t � o      ���� $0 emacsclientpath2 emacsclientPath2 �  � � � r     � � � m    	 � � � � �   � o      ���� ,0 foundemacsclientpath foundEmacsClientPath �  � � � Q    ` � � � � k     � �  � � � I   �� ���
�� .sysoexecTEXT���     TEXT � b     � � � m     � � � � �  t e s t   - x   � n     � � � 1    ��
�� 
strq � o    ���� $0 emacsclientpath1 emacsclientPath1��   �  ��� � r     � � � o    ���� $0 emacsclientpath1 emacsclientPath1 � o      ���� ,0 foundemacsclientpath foundEmacsClientPath��   � R      ������
�� .ascrerr ****      � ****��  ��   � k   $ ` � �  � � � l  $ $�� � ���   � 1 + If first path fails, check the second path    � � � � V   I f   f i r s t   p a t h   f a i l s ,   c h e c k   t h e   s e c o n d   p a t h �  ��� � Q   $ ` � � � � k   ' 4 � �  � � � I  ' 0�� ���
�� .sysoexecTEXT���     TEXT � b   ' , � � � m   ' ( � � � � �  t e s t   - x   � n   ( + � � � 1   ) +��
�� 
strq � o   ( )���� $0 emacsclientpath2 emacsclientPath2��   �  ��� � r   1 4 � � � o   1 2���� $0 emacsclientpath2 emacsclientPath2 � o      ���� ,0 foundemacsclientpath foundEmacsClientPath��   � R      ������
�� .ascrerr ****      � ****��  ��   � k   < ` � �  � � � l  < <�� � ���   � : 4 If neither path is found, display an error and exit    � � � � h   I f   n e i t h e r   p a t h   i s   f o u n d ,   d i s p l a y   a n   e r r o r   a n d   e x i t �  � � � I  < ]�� � �
�� .sysodlogaskr        TEXT � b   < E � � � b   < C � � � b   < A � � � b   < ? � � � m   < = � � � � � d E r r o r :   e m a c s c l i e n t   e x e c u t a b l e   n o t   f o u n d   a t   e i t h e r   � o   = >���� $0 emacsclientpath1 emacsclientPath1 � m   ? @ � � � � �    o r   � o   A B���� $0 emacsclientpath2 emacsclientPath2 � m   C D � � � � �  . � �� � �
�� 
appr � m   F G � � � � � * E m a c s C l i e n t   N o t   F o u n d � �� � �
�� 
btns � J   H K � �  ��� � m   H I � � � � �  O K��   � �� � �
�� 
dflt � m   N Q � � � � �  O K � �� ���
�� 
disp � m   T W��
�� stic    ��   �  ��� � L   ^ `����  ��  ��   �  � � � l  a a��������  ��  ��   �  � � � r   a j � � � b   a h � � � n   a d � � � 1   b d��
�� 
strq � o   a b���� ,0 foundemacsclientpath foundEmacsClientPath � m   d g � � � � �    - - n o - w a i t   � o      ���� 0 ec EC �  � � � r   k u � � � n   k s � � � 1   q s��
�� 
strq � I   k q�� ����� 0 
converturl 
convertURL �  ��� � o   l m���� 0 this_url this_URL��  ��   � o      ���� 0 filepath filePath �  � � � I  v }�� ���
�� .sysoexecTEXT���     TEXT � b   v y � � � o   v w���� 0 ec EC � o   w x���� 0 filepath filePath��   �  ��� � O  ~ � � � � I  � �������
�� .miscactvnull��� ��� null��  ��   � m   ~ � � �|                                                                                  EMAx  alis      Macintosh HD               �P�BD ����	Emacs.app                                                      ������B         ����  
 cu             Applications  /:Applications:Emacs.app/    	 E m a c s . a p p    M a c i n t o s h   H D  Applications/Emacs.app  / ��  ��   o  � � � l     ��������  ��  ��   �  ��� � l     �� � ���   � G A open location "org-protocol://gtd-capture://r/test/test-tittle/"    � � � � �   o p e n   l o c a t i o n   " o r g - p r o t o c o l : / / g t d - c a p t u r e : / / r / t e s t / t e s t - t i t t l e / "��       �� � ��   � ������ 0 
converturl 
convertURL
�� .GURLGURLnull��� ��� TEXT  �� �������� 0 
converturl 
convertURL�� ����   ���� 0 inputurl inputURL��   ������ 0 inputurl inputURL�� 0 convertedurl convertedURL  ,����� B�~ [
�� 
ascr
�� 
txdl
� 
citm
�~ 
ctxt�� 9���,FO��l/� $�[�\[Zk\Zl2�&�%�[�\[Zm\Zi2%�&E�Y �E�O� �} q�|�{�z
�} .GURLGURLnull��� ��� TEXT�| 0 this_url this_URL�{   �y�x�w�v�u�t�y 0 this_url this_URL�x $0 emacsclientpath1 emacsclientPath1�w $0 emacsclientpath2 emacsclientPath2�v ,0 foundemacsclientpath foundEmacsClientPath�u 0 ec EC�t 0 filepath filePath  } � � ��s�r�q�p � � � ��o ��n ��m ��l�k�j�i ��h ��g
�s 
strq
�r .sysoexecTEXT���     TEXT�q  �p  
�o 
appr
�n 
btns
�m 
dflt
�l 
disp
�k stic    �j 
�i .sysodlogaskr        TEXT�h 0 
converturl 
convertURL
�g .miscactvnull��� ��� null�z ��E�O�E�O�E�O ��,%j O�E�W CX   ��,%j O�E�W +X  �%�%�%�%����kva a a a a  OhO��,a %E�O*�k+ �,E�O��%j Oa  *j Uascr  ��ޭ