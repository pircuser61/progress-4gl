/* сортировка выполняется при клике на столбце,
 фильтрция - для не пустых поуле в момент установки чекбокса
 "на лету" фильтры не обновляются, необходимо сбросить и заново 
 установить флажок"*/

&SCOPED-DEFINE h_ 22 /*высота окна */
&SCOPED-DEFINE w_ 210/*ширина окна */
&SCOPED-DEFINE w_kat 70 /*ширина дерева */
&SCOPED-DEFINE h_kat 120 /*начальная высота  дерева (для инициализации field-group)*/
&SCOPED-DEFINE w_btn 40 /*ширина кнопки дерева */
&SCOPED-DEFINE w_kat_in 4 /*ширина отступа в дереве */
&SCOPED-DEFINE w_data 138 /*  ширина BROWSE*/

DEFINE VARIABLE rw AS INTEGER INITIAL 120.

&SCOPED-DEFINE query_string  "FOR EACH katalog_of_classf " /* запрос к временной таблице */

 
/* temp-table */
/* хотел использовать work-table но они не поддерживают query:prepare */
DEFINE TEMP-TABLE katalog_of_classf LIKE katalog. 
 
/* QUERY  & var*/ 
DEFINE QUERY q_katalog FOR katalog_of_classf.

DEFINE VARIABLE sort_string AS CHARACTER.
DEFINE VARIABLE filter_string AS CHARACTER.
 
/* WIDGETS & var */
DEFINE VARIABLE filter_name AS CHARACTER LABEL "Название".
DEFINE VARIABLE filter_artic AS CHARACTER LABEL "Артикул".
DEFINE VARIABLE filter_cod_firm AS CHARACTER  LABEL "Код поставщика".
DEFINE VARIABLE filter_apply  AS LOGICAL VIEW-AS TOGGLE-BOX 
    INITIAL TRUE LABEL "Применить фильтры".

DEFINE BUTTON btn_root LABEL "+"  SIZE 4 BY 1.
DEFINE BUTTON btn_root_text LABEL "Без категории" SIZE {&w_btn} BY 1.
DEFINE BUTTON btn_exit LABEL "Выход".
 

DEFINE BROWSE b_katalog QUERY q_katalog  
    DISPLAY  artic NAME  cod_good  cod_firm  
    ENABLE artic NAME  cod_good   
    WITH 22 DOWN  WIDTH {&w_data}  SEPARATORS.

/* frames */
DEFINE FRAME main 
    SPACE( {&w_kat} ) SPACE(1)  b_katalog SKIP(1)    
    SPACE( {&w_kat} ) SPACE(1)  
    filter_artic  
    filter_name  
    filter_cod_firm  
    filter_apply
    
    SPACE SPACE(5) btn_exit   
     
    WITH  SIDE-LABELS  SIZE {&w_} BY {&h_}.

DEFINE FRAME kat
    btn_root COLON 1
    btn_root_text  COLON 5
    WITH OVERLAY SIZE {&w_kat} BY {&h_kat}.

/* init ui */
ASSIGN
    DEFAULT-WINDOW:WIDTH ={&w_}  
    DEFAULT-WINDOW:HEIGHT = {&h_}  
     
    FRAME kat:FRAME = FRAME main:HANDLE 
     
    artic:READ-ONLY IN BROWSE b_katalog = TRUE
    name:READ-ONLY IN BROWSE b_katalog = TRUE
    cod_good:READ-ONLY IN BROWSE b_katalog = TRUE.   
   
/* triggers */
/* список категорий без родителя */
btn_root:PRIVATE-DATA =  STRING(0) + ",".
ON CHOOSE OF btn_root IN FRAME kat PERSISTENT RUN show_sub_classf IN THIS-PROCEDURE.
              
/* список товаров без категории */
btn_root_text:PRIVATE-DATA = STRING(0).
ON CHOOSE OF btn_root_text IN  FRAME kat PERSISTENT RUN show_katalog IN THIS-PROCEDURE.
            
 
ON VALUE-CHANGED OF filter_apply IN FRAME main DO:
    /* формирует строку фильтрации и выполняет запрос к katalog_of_classf*/
    filter_string = "".
    IF filter_apply:CHECKED THEN DO:
        ASSIGN  filter_name.
        ASSIGN filter_artic.
        ASSIGN filter_cod_firm.
         
        IF "" <> filter_name  THEN 
            filter_string = filter_string + "  name  matches (""*" +  filter_name  + "*"") and".
        IF "" <> filter_artic  THEN 
            filter_string = filter_string + "  artic  matches (""*" +  filter_artic  + "*"") and".
        IF "" <>  filter_cod_firm  THEN DO:
            filter_string = filter_string + "  cod_firm = " + string(integer(filter_cod_firm)) + " and" NO-ERROR.
            IF  ERROR-STATUS:ERROR THEN DO:
                MESSAGE "Код производителя должен быть целым числом"
                     VIEW-AS ALERT-BOX INFORMATION BUTTON OK.
                RETURN.
                END.
            END.
        DEFINE VARIABLE len AS INTEGER.
        len = LENGTH(filter_string).
        IF len > 0 THEN  /* отсекаем последний and и добалякм where */
            filter_string = " where " +  substring(filter_string,1,len - 3).
        END.

    QUERY q_katalog:QUERY-CLOSE(). 
    QUERY q_katalog:QUERY-PREPARE({&query_string} + filter_string + sort_string). 
    QUERY q_katalog:QUERY-OPEN(). 
    END.


ON START-SEARCH OF b_katalog DO:
    /* формирует строку сортировки и выполняет запрос к katalog_of_classf*/ 
    /* если PRIVATE-DATA выбранной колонки пустая строка - по возрастанию*/
   
    DEFINE VARIABLE h_col  AS WIDGET-HANDLE.
    h_col = BROWSE b_katalog:CURRENT-COLUMN.
    MESSAGE h_col:PRIVATE-DATA .
    sort_string = " by " + h_col:NAME.
    
  
    IF h_col:PRIVATE-DATA = "" THEN  DO:  
        sort_string  =  sort_string  + " DESCENDING".
        h_col:PRIVATE-DATA  = "1".
        END.
    ELSE h_col:PRIVATE-DATA  = "". 

    QUERY q_katalog:QUERY-CLOSE(). 
    QUERY q_katalog:QUERY-PREPARE({&query_string} + filter_string + sort_string).
    QUERY q_katalog:QUERY-OPEN(). 
END.

  
/* main */
 
VIEW FRAME MAIN.
ENABLE ALL WITH FRAME kat.
ENABLE ALL WITH FRAME main.
APPLY "choose" TO btn_root IN FRAME kat.

WAIT-FOR CHOOSE OF btn_exit.


/*functions */
PROCEDURE show_katalog.
    /*сохраняет во временную табицу  список товаров выбранной категории
    и выводит содержимое */
    DEFINE VARIABLE classf_id AS INTEGER.

    classf_id = integer(ENTRY(1,SELF:PRIVATE-DATA)).
    
    FOR EACH katalog_of_classf:
        DELETE katalog_of_classf.
        END.
    FOR EACH cl-good WHERE cl-good.id = classf_id  NO-LOCK,
        EACH katalog OF cl-good  NO-LOCK:
            CREATE katalog_of_classf.
            BUFFER-COPY katalog TO katalog_of_classf.
        END.
        
    QUERY q_katalog:QUERY-CLOSE(). 
    QUERY q_katalog:QUERY-PREPARE({&query_string} + filter_string + sort_string). 
    QUERY q_katalog:QUERY-OPEN(). 
END PROCEDURE.


PROCEDURE show_sub_classf.
    /* отображение / скрытие подкатегорий */   
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
                VIEW hChild  .
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
 /* Создает фрейм hFrame и кнопки в нем для прямых потомков classf с id =id_
    первый элемент BUTTON:PRIVATE-DATA  - id соответствующего classf 
    возвращает количество созданных кнопок + 1 в i
    если потомков нет - фрейм удаляется, i = 0*/
    
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
            LABEL =  NAME 
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
 
        hFrame:WIDTH = hParent:WIDTH - {&w_kat_in}  NO-ERROR .
        hFrame:ROW = self:ROW + 1  NO-ERROR .
        hFrame:HEIGHT = i   NO-ERROR .
        END.
    ELSE DO:
        SELF:PRIVATE-DATA = STRING( id_) + "," + STRING(0).
        DELETE WIDGET hFrame.
        END.
            
END PROCEDURE.


PROCEDURE resize_frame.
/* сдвигает виджеты расположенные ниже pos в фрейме hFrame на shift строк
   растягивает окно hFrame на shift
   предварительно вызывается для всех родителей до фрейма kat
   во фрейме kat виджеты сдвигаются но сам фрейм не растягиваетя
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


/*хотел добавить возможность просмотра товаров в подкаталогах
*/
/*
PROCEDURE classf_items.
    /* списсок подкаталогов  */
    DEF INPUT PARAMETER id_ AS INTEGER.
    DEF INPUT-OUTPUT  PARAMETER res AS CHARACTER.
    res = res + STRING(id_) + ", ".
    FOR EACH classf WHERE parent_id = id_:
        RUN classf_items(id,INPUT-OUTPUT  res).
    END.
END PROCEDURE.

PROCEDURE get_katalog_by_classf_list.
     /*товары для списка подкаталогово*/
    DEFINE INPUT PARAMETER list AS CHARACTER.
    DEFINE VARIABLE id_ AS INTEGER.
    DEFINE VARIABLE i  AS INTEGER.

    FOR EACH katalog_of_classf:
        DELETE katalog_of_classf.
        END.
  
    DO i = 1 TO NUM-ENTRIES(list) - 1: /* список заканчивается запятой
                                         последний элемент пустой*/
       id_ = INTEGER( ENTRY( i, list )).
       FOR EACH cl-good WHERE  cl-good.id = id_,
            EACH katalog OF cl-good:
                CREATE katalog_of_classf.
                BUFFER-COPY katalog TO katalog_of_classf.
            END.
       END.
END PROCEDURE.

PROCEDURE show_katalog.
    /* вывод товаров каталога или списка каталогов */
    DEFINE VARIABLE classf_id AS INTEGER.

    classf_id = integer(ENTRY(1,SELF:PRIVATE-DATA)).
    IF show_sub:CHECKED IN FRAME main THEN DO:
        IF classf_id = 0  THEN RETURN.
        DEFINE VARIABLE res AS CHARACTER.
        RUN classf_items(classf_id, INPUT-OUTPUT res).
        RUN get_katalog_by_classf_list(res).
        END.
        
    ELSE RUN get_katalog_by_classf(classf_id).
        
    QUERY q_katalog:QUERY-CLOSE(). 
    QUERY q_katalog:QUERY-PREPARE({&query_string} + filter_string + sort_string). 
    QUERY q_katalog:QUERY-OPEN(). 
END PROCEDURE.
*/
