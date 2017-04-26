&SCOPED-DEFINE h_ 24 /*высота окна */
&SCOPED-DEFINE w_ 240/*ширина окна */
&SCOPED-DEFINE w_kat 70 /*ширина дерева */
&SCOPED-DEFINE h_kat 120 /*начальная высота  дерева (для инициализации field-group)*/
&SCOPED-DEFINE w_btn 50 /*ширина кнопки дерева */
&SCOPED-DEFINE w_kat_in 4 /*ширина отступа в дереве */
&SCOPED-DEFINE w_data 165 /*  ширина BROWSE*/
 
DEFINE QUERY q_katalog FOR  cl-good, katalog.

/* WIDGETS */
DEFINE BUTTON btn_exit LABEL "EXIT".

DEFINE BROWSE b_katalog QUERY q_katalog  
    DISPLAY  artic katalog.NAME  price_name katalog.cod_good  cod_firm  
    WITH 24 DOWN  WIDTH {&w_data}  SEPARATORS.
/* frames */
DEFINE FRAME main 
     b_katalog 
    btn_exit 
    WITH   WIDTH {&w_}.
DEFINE FRAME kat 
    WITH   OVERLAY.

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
    btn_exit:ROW =  {&h_} - 1.


    DEFINE VARIABLE tmp AS INTEGER.
    RUN show_childs(0, FRAME kat:HANDLE,OUTPUT tmp). 
    VIEW FRAME MAIN.
    

ENABLE ALL WITH FRAME main.
WAIT-FOR CHOOSE OF btn_exit.
 

/*functions ======================================================= */

PROCEDURE show_sub_classf.
   
    DEFINE VARIABLE hParent AS WIDGET-HANDLE.
    DEFINE VARIABLE hFrame AS WIDGET-HANDLE.
    DEFINE VARIABLE hChild AS WIDGET-HANDLE.
    DEFINE VARIABLE shift AS INTEGER.
    DEFINE VARIABLE classf_id AS INTEGER.
 
    hParent = SELF:FRAME.
    classf_id = integer(entry(1,SELF:PRIVATE-DATA)).
    hChild = WIDGET-HANDLE( entry(2,SELF:PRIVATE-DATA)).

    CASE SELF:LABEL :
        WHEN "+"  THEN DO:
        IF VALID-HANDLE(hChild) THEN DO: 
            hFrame = hChild.
            shift = hChild:HEIGHT.
            END.
        ELSE DO:
            CREATE FRAME hFrame ASSIGN
                FRAME = SELF:FRAME
                OVERLAY =TRUE
                HIDDEN = TRUE
                /* BOX = FALSE */
                SCROLLABLE = FALSE
                COL = {&w_kat_in}.

           
            hFrame:WIDTH = hParent:WIDTH - hFrame:COL.
            hFrame:ROW = self:ROW + {&w_kat_in}.

            RUN show_childs(classf_id, hFrame, OUTPUT shift).
            END.
    
        IF shift > 1 THEN DO:
            SELF:PRIVATE-DATA = STRING(classf_id) + "," + STRING(hFrame).
            SELF:LABEL = "-". 
           
            RUN resize_frame(SELF:FRAME, SELF:ROW, shift).
             
            hFrame:HEIGHT = shift.
            hFrame:ROW = SELF:ROW + 1.
           
            VIEW hFrame.
            END.
        ELSE DO:
            SELF:PRIVATE-DATA = STRING(classf_id) + "," + STRING(0).
            SELF:LABEL = "".
            DELETE WIDGET hFrame.
            END.    
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


PROCEDURE resize_frame.
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

/*========================================================*/
PROCEDURE show_katalog.
    DEFINE VARIABLE classf_id AS INTEGER.
    classf_id = integer(ENTRY(1,SELF:PRIVATE-DATA)).
 
    OPEN QUERY q_katalog FOR 
        EACH cl-good WHERE cl-good.id = classf_id, 
        EACH katalog OF cl-good.

    
END PROCEDURE.



PROCEDURE show_childs:
    DEFINE INPUT PARAMETER id_ AS INTEGER.
    DEFINE INPUT PARAMETER hFrame AS WIDGET-HANDLE.
    DEFINE OUTPUT PARAMETER i AS INTEGER INITIAL 0.
   
    DEFINE VARIABLE  hBtn AS WIDGET-HANDLE.
 
    FOR EACH classf WHERE parent_id = id_:
        
        i = i + 1.
        CREATE BUTTON hBtn ASSIGN 
            LABEL = "+"
            FRAME = hFrame
            ROW = i 
            COL = 1
            WIDTH = 4
            SENSITIVE = TRUE
            PRIVATE-DATA =  STRING(id) + ","
            TRIGGERS:
                ON CHOOSE PERSISTENT RUN show_sub_classf IN THIS-PROCEDURE.
                END TRIGGERS.

        CREATE BUTTON hBtn ASSIGN 
            LABEL =STRING(i) + "  " +  NAME
            FRAME = hFrame
            ROW = i 
            COL = 5
            WIDTH = {&w_btn}
            SENSITIVE = TRUE
            PRIVATE-DATA = STRING(id)
            TRIGGERS:
                ON CHOOSE PERSISTENT RUN show_katalog IN THIS-PROCEDURE.
                END TRIGGERS.

        END.
        i = i + 1.    
END PROCEDURE.
