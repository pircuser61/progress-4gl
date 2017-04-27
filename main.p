&SCOPED-DEFINE h_ 24 /*высота окна */
&SCOPED-DEFINE w_ 240/*ширина окна */
&SCOPED-DEFINE w_kat 70 /*ширина дерева */
&SCOPED-DEFINE h_kat 120 /*начальная высота  дерева (для инициализации field-group)*/
&SCOPED-DEFINE w_btn 50 /*ширина кнопки дерева */
&SCOPED-DEFINE w_kat_in 4 /*ширина отступа в дереве */
&SCOPED-DEFINE w_data 165 /*  ширина BROWSE*/



/* work table */
DEFINE WORK-TABLE katalog_of_classf LIKE katalog.

/* QUERY  */ 
DEFINE QUERY q_katalog FOR katalog_of_classf.
 
/* WIDGETS & var */
DEFINE BUTTON btn_root LABEL "+".
DEFINE BUTTON btn_root_text LABEL "Без категории" SIZE {&w_btn} BY 1.
DEFINE BUTTON btn_exit LABEL "Выход".
DEFINE BUTTON btn_filter LABEL "Фильтры".
DEFINE VARIABLE show_sub AS LOGICAL VIEW-AS TOGGLE-BOX 
    INITIAL TRUE LABEL "Отображать содержимое подкатегорий".

DEFINE BROWSE b_katalog QUERY q_katalog  
    DISPLAY  artic NAME  cod_good  cod_firm  
    ENABLE artic NAME  cod_good   
    WITH 24 DOWN  WIDTH {&w_data}  SEPARATORS.

/* frames */
DEFINE FRAME main 
    b_katalog 
    btn_exit 
    show_sub
    btn_filter
    WITH   WIDTH {&w_}.
DEFINE FRAME kat
    btn_root
    btn_root_text
    WITH OVERLAY.

/* init ui */
ASSIGN
    DEFAULT-WINDOW:WIDTH ={&w_}
    DEFAULT-WINDOW:HEIGHT = {&h_}
    
    FRAME main:HEIGHT = {&h_}
    FRAME main:WIDTH = {&w_}
    
    FRAME kat:FRAME = FRAME main:HANDLE
    FRAME kat:WIDTH = {&w_kat}
    FRAME kat:HEIGHT = {&h_kat}
 
    b_katalog:COL = {&w_kat} + 2
    btn_exit:COL =  {&w_} - 10      
    btn_exit:ROW =  {&h_} - 1

    btn_filter:COL =  {&w_kat} + 4       
    btn_filter:ROW =  {&h_} - 1 

    show_sub:COL =  {&w_kat} + 24     
    show_sub:ROW =  {&h_} - 1 

    artic:READ-ONLY IN BROWSE b_katalog = TRUE
    name:READ-ONLY IN BROWSE b_katalog = TRUE
    cod_good:READ-ONLY IN BROWSE b_katalog = TRUE
    
    btn_root:COL = 1
    btn_root:ROW = 1
    btn_root_text:COL = 4
    btn_root_text:ROW = 1.

 
 /* triggers */

btn_root:PRIVATE-DATA =  STRING(0) + ",".
ON CHOOSE OF btn_root IN FRAME kat PERSISTENT RUN show_sub_classf IN THIS-PROCEDURE.
              

btn_root_text:PRIVATE-DATA = STRING(0).
ON CHOOSE OF btn_root_text IN  FRAME kat PERSISTENT RUN show_katalog IN THIS-PROCEDURE.
            

ON START-SEARCH OF b_katalog DO:
    /* сортировка при клике на колонке BROWSE */
    DEFINE VARIABLE h_col  AS WIDGET-HANDLE.
    h_col = BROWSE b_katalog:CURRENT-COLUMN.

    IF h_col:PRIVATE-DATA = "" THEN DO: /* по возрастанию */
        h_col:PRIVATE-DATA  = "1".
        CASE h_col:NAME:
            WHEN "name" THEN OPEN QUERY q_katalog FOR 
                EACH katalog_of_classf BY NAME.
            WHEN "artic" THEN OPEN QUERY q_katalog FOR 
                EACH katalog_of_classf BY artic.     
            WHEN "cod_good" THEN OPEN QUERY q_katalog FOR 
                EACH katalog_of_classf BY cod_good.
            END CASE.
        END.

    ELSE DO:
        h_col:PRIVATE-DATA  = "".
        CASE h_col:NAME:
            WHEN "name" THEN OPEN QUERY q_katalog FOR 
                EACH katalog_of_classf BY NAME DESCENDING.
            WHEN "artic" THEN OPEN QUERY q_katalog FOR 
                EACH katalog_of_classf BY artic DESCENDING.     
            WHEN "cod_good" THEN OPEN QUERY q_katalog FOR 
                EACH katalog_of_classf BY cod_good DESCENDING.
            END CASE.
        END.

/* краш без сообщений
  DEFINE VARIABLE h_col  AS WIDGET-HANDLE.
  DEFINE VARIABLE h_query AS HANDLE.

  h_col = BROWSE b_katalog:CURRENT-COLUMN.
  h_query  = BROWSE b_katalog:QUERY.    
  h_query:QUERY-CLOSE(). 
  h_query:QUERY-PREPARE("FOR EACH katalog_of_classf").
  h_query:QUERY-OPEN(). 
  */
 
END.

  

VIEW FRAME MAIN.

    
ENABLE ALL WITH FRAME kat.
ENABLE ALL WITH FRAME main.
WAIT-FOR CHOOSE OF btn_exit.


/*functions */
PROCEDURE get_katalog_by_classf.
     /* список товаров заданного каталога*/
    DEFINE INPUT PARAMETER id_ AS INTEGER.
    
    FOR EACH katalog_of_classf:
        DELETE katalog_of_classf.
        END.
    FOR EACH cl-good WHERE cl-good.id = id_,
        EACH katalog OF cl-good:
            CREATE katalog_of_classf.
            BUFFER-COPY katalog TO katalog_of_classf.
        END.
END PROCEDURE.

PROCEDURE get_katalog_by_classf_list.
     /* список  товаров из списка каталогов list*/
    DEFINE INPUT PARAMETER list AS CHARACTER.
    DEFINE VARIABLE id_ AS INTEGER.
    DEFINE VARIABLE i  AS INTEGER.

    FOR EACH katalog_of_classf:
        DELETE katalog_of_classf.
        END.
  
    DO i = 1 TO NUM-ENTRIES(list):
       IF  i = 0 OR i > 6 THEN RETURN.

       id_ = INTEGER( ENTRY( i, list )).
    

       FOR EACH cl-good WHERE  cl-good.id = id_,
            EACH katalog OF cl-good:
                CREATE katalog_of_classf.
                BUFFER-COPY katalog TO katalog_of_classf.
            END.
       END.

END PROCEDURE.

PROCEDURE show_katalog.
    /* список товаров заданного каталога classf_id*/
    DEFINE VARIABLE classf_id AS INTEGER.

    classf_id = integer(ENTRY(1,SELF:PRIVATE-DATA)).
    IF show_sub:CHECKED IN FRAME main THEN DO:
        DEFINE VARIABLE res AS CHARACTER.
        RUN classf_items(classf_id, INPUT-OUTPUT   res).
        RUN get_katalog_by_classf_list(res).
    END.
        
    ELSE RUN get_katalog_by_classf(classf_id).
        
    OPEN QUERY q_katalog FOR EACH katalog_of_classf. 
END PROCEDURE.


PROCEDURE classf_items.
    /* список id подкатегорий для категории с id = id_ */
    DEF INPUT PARAMETER id_ AS INTEGER.
    DEF INPUT-OUTPUT  PARAMETER res AS CHARACTER.
    res = res + STRING(id_) + ", ".
    FOR EACH classf WHERE parent_id = id_:
        RUN classf_items(id,INPUT-OUTPUT  res).
    END.
END PROCEDURE.



/*====      UI     ====*/
PROCEDURE show_sub_classf.
/* обработчик нажатия кнопки в дереве категорий
   если label кнопки "+":   получает список подкатегорий
        и создает дочерний фреййм с кнопками,
        либо отображает ранее созданный фрейм.
        Меняет надпись на "-" если список подкатегорий не пуст,
        если список пуст ферйм тут же удаляется, надпись на кнопеке стирается.
   если label кнопки "+": скрывает дочернй фрейм 
   если label кнопки "":не делает ничего 
*/   
   
 
    DEFINE VARIABLE hChild AS WIDGET-HANDLE.
    DEFINE VARIABLE shift AS INTEGER.
    DEFINE VARIABLE classf_id AS INTEGER.
 
    
    
    hChild = WIDGET-HANDLE( entry(2,SELF:PRIVATE-DATA)).

    CASE SELF:LABEL :
        WHEN "+"  THEN DO:

            IF VALID-HANDLE(hChild) THEN shift = hChild:HEIGHT.
            ELSE RUN show_childs(OUTPUT hChild,  OUTPUT shift).
          
            IF shift > 1 THEN DO:
                SELF:LABEL = "-". 
               
                RUN resize_frame(SELF:FRAME, SELF:ROW, shift).
                 
               
                hChild:ROW = SELF:ROW + 1.
             
                 
                VIEW hChild.

                END.
            ELSE SELF:LABEL = "".
        END.

        WHEN "-" THEN DO:
            SELF:LABEL = "+".  
            
            IF VALID-HANDLE(hChild) THEN DO:
                hChild:HIDDEN = TRUE.
                shift = - hChild:HEIGHT.
                RUN resize_frame(SELF:FRAME, SELF:ROW, shift).
                END.
            END.
        END CASE.
END PROCEDURE. 


PROCEDURE show_childs:
 /* Создает кнопки во фрейме hFrame для прямых потомков classf с id =id_
    первый элемент BUTTON:PRIVATE-DATA  - id соответствующего classf 
    возвращает количество созданных кнопок в i*/
    
    DEFINE OUTPUT PARAMETER hFrame AS HANDLE.
    DEFINE OUTPUT PARAMETER i AS INTEGER INITIAL 0.
   
    DEFINE VARIABLE  id_ AS INTEGER.
    DEFINE VARIABLE  hBtn AS WIDGET-HANDLE.
    DEFINE VARIABLE hParent AS WIDGET-HANDLE.

    id_ = integer(entry(1,SELF:PRIVATE-DATA)).
    
    hParent = SELF:FRAME.


    CREATE FRAME hFrame ASSIGN
        FRAME = SELF:FRAME
        OVERLAY =TRUE
        HIDDEN = TRUE
        /* BOX = FALSE */
        SCROLLABLE = FALSE
        COL = {&w_kat_in}.

 
 
    FOR EACH classf WHERE parent_id = id_ NO-LOCK:
        
        
        CREATE BUTTON hBtn NO-ERROR ASSIGN 
            LABEL = "+"
            FRAME = hFrame
            ROW = i + 1 
            COL = 1
            WIDTH = 4
            SENSITIVE = TRUE
            PRIVATE-DATA =  STRING(id) + ","
            TRIGGERS:
                ON CHOOSE PERSISTENT RUN show_sub_classf IN THIS-PROCEDURE.
                END TRIGGERS.

        CREATE BUTTON hBtn  NO-ERROR ASSIGN 
            LABEL =STRING(id) + "  " +  NAME 
            FRAME = hFrame
            ROW = i + 1 
            COL = 5
            WIDTH = {&w_btn}
            SENSITIVE = TRUE
            PRIVATE-DATA = STRING(id)
            TRIGGERS:
                ON CHOOSE PERSISTENT RUN show_katalog IN THIS-PROCEDURE.
                END TRIGGERS.
        i = i + 1.
        END.
      
    IF i > 0 THEN DO:   
          i = i + 1.
        SELF:PRIVATE-DATA = STRING(id_) + "," + STRING(hFrame).
 
        hFrame:WIDTH = hParent:WIDTH - hFrame:COL  NO-ERROR .
        hFrame:ROW = self:ROW + 1  NO-ERROR .
        hFrame:HEIGHT = i   NO-ERROR .
        END.
    ELSE DO:
        SELF:PRIVATE-DATA = STRING( id_) + "," + STRING(0).
        DELETE WIDGET hFrame.
        END.
            
END PROCEDURE.


PROCEDURE resize_frame.
/* сдвигает виджеты расположенные ниже pos в окне hFrame на shift строк
   растягивает окно hFrame на shift
   предварительно вызывается для всех родителей до фрейма kat
   во фрейме kat виджеты сдвигаются но сам фрейм не раятягиваетя
   shift может быть отрицательным
 */
    DEFINE INPUT PARAMETER hFrame AS WIDGET-HANDLE.
    DEFINE INPUT PARAMETER pos AS INTEGER.
    DEFINE INPUT PARAMETER shift AS INTEGER.

    DEFINE VARIABLE hW AS WIDGET-HANDLE.
    DEFINE VARIABLE hParent AS WIDGET-HANDLE.
  

    IF hFrame <> FRAME kat:HANDLE THEN RUN resize_frame(hFrame:FRAME, hFrame:ROW, shift).
   
    hw = hFrame:FIRST-CHILD. /* field-group */
    hw = hw:FIRST-CHILD. /* button */
    DO WHILE VALID-HANDLE(hW):
        IF hw:ROW > pos THEN hW:ROW = hW:ROW + shift NO-ERROR.
        hw = hw:NEXT-SIBLING.
        END.
 
    IF hFrame <> FRAME kat:HANDLE THEN  hFrame:HEIGHT-CHARS = hFrame:HEIGHT-CHARS + shift.
END PROCEDURE.

