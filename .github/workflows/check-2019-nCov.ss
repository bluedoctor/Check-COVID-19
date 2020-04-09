; 2019新型冠状病毒肺炎（COVID-19）感染风险自助检测程序
(define A1 (list "发热" (cons "三天内" 5) (cons "三天到一周" 10) (cons "超过一周" 15)))
(define A2 (list "咳嗽" (cons "无痰" 15) (cons "有痰难吐" 10) (cons "有痰易吐" -10)))
(define A3 (list "乏力" (cons "无" -15) (cons "轻微" 15) (cons "明显" 30)))
(define A4 (list "腹泻" (cons "无" 0) (cons "轻微" 10) (cons "明显" 5)))
(define A5 (list "呼吸困难" (cons "无" 0) (cons "略感胸闷" 15) (cons "明显" 30)))
(define QA (vector A1 A2 A3 A4 A5))

(define B1 (list "胸部CT" (cons "正常/未检测" 1) (cons "肺部毛玻璃样" 80) (cons "其它情况" 20)))
(define B2 (list "病毒核酸检测" (cons "未检测" 5) (cons "阳性" 100) (cons "阴性" 20)))
(define B3 (list "白细胞计数" (cons "正常/未检测" 1) (cons "偏低" 20) (cons "增高" -20)))
(define QB (vector B1 B2 B3 ))

(define Total_Risk 0) ;定义全局 风险总值 变量
(define Ctx_Attributes '())

(define (show-attribute An)
    ;循环输出点对数据
    (do (   (name-values (cdr An) (cdr name-values))
        (i 1 (+ i 1)))
        ((null? name-values) (- i 1))  
        (display i) 
        (display ",")
        (display (car (car name-values)))    
        (display #\space)  
    )
)

(define (input_selected )
    (let loop ()
        (display "请输入你选择的答案对应的数字:")  
        (let ((k (read)))   
            (if (integer? k)    
                k ;return
                (begin
                     (display "输入错误!")  
                     (newline)
                     (loop))))))

;获取症状特征列表指定的特征序号（从1开始的序号）对应的风险值
(define (get-index-value lst_attribute index)
    (do ((name-values (cdr lst_attribute) (cdr name-values)) 
         (i 1 (+ i 1)))
        ( (or (= i index) (null? (cdr name-values))) ;break
          (cdr (car name-values)) ;return value
        )
        ;(display (cdr name-values ))   
        ;(newline) 
    )
)

(define (get-index-name lst_attribute index)
    (do ((name-values (cdr lst_attribute) (cdr name-values)) 
         (i 1 (+ i 1)))
        ( (or (= i index) (null? (cdr name-values))) ;break
          (car (car name-values)) ;return value
        )
        ;(display (cdr name-values ))   
        ;(newline) 
    )
)

;遍历所有的可能症状
(define (process-question listAttributes)
    (let loop ((i 0) (j (- (vector-length listAttributes) 1)))
    (display (+ i 1))
    (display ",您最近是否有【")
    (let ((Ai (vector-ref listAttributes i)))
        (display (car Ai))
        (display "】的情况?(如果有，请输入数字1；否则输入其它字符以跳过此项检测。)")
        (newline)
        ;(let ((input 1)) ;test
        (let ((input (read)) (Ai_Name (car Ai)))
            (if (and (integer? input) (= input 1))
                (begin
                    (display Ai_Name)
                    (display "的具体情况是：")
                    (show-attribute Ai)
                    (newline)
                    (let ((q_index (input_selected )))
                        (display "您当前选择的情况风险值是：")
                        (let ((curr_risk (get-index-value Ai q_index))) 
                            (display curr_risk)
                            (set! Ctx_Attributes (append Ctx_Attributes 
                                (list (list Ai_Name (get-index-name Ai q_index) curr_risk ))))
                            (set! Total_Risk (+ Total_Risk curr_risk)) 
                        )
                        
                    ) 
                )
                (begin
                    (display "您没有【")
                    (display Ai_Name)
                    (display "】的症状。")
                )
            )
        )
        (newline)
        (newline)
    )

    (if (< i j)
        (loop (+ i 1) j))
))


;Ctx_Attributes 结果示例：
;   ((发热 三天内 5) (咳嗽 无痰 15) (乏力 无 -15) (腹泻 无 0) (呼吸困难 无 0))

;从结果中判断是否有指定的症状属性；如果有，返回症状特征表；如果没有，返回空表
(define (have_attribute_inResult result attName)
    (let loop ( (lst result) )
        (let ( (car_lst (car lst))  (cdr_lst (cdr lst)))
            ;(if (equal? attName (car car_lst))
            ;    car_lst
            ;     (if (null? cdr_lst)
            ;        '()
            ;        (loop cdr_lst)
            ;    )
            ;)
            ;
            (cond   ((equal? attName (car car_lst))  car_lst )
                    ((null? cdr_lst) '())
                    (else (loop cdr_lst))
            )
        )
    )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(display "========新冠病毒肺炎(COVID-19)诊断检测实验程序============")(newline)
(display "**    注意：本程序仅为演示Scheme计算机语言编程使用       **")(newline)
(display "---------作者：深蓝医生 2020-4-5..9  ----------------------")(newline)
(display "---------http://www.cnblogs.com/bluedoctor  ---------------")(newline)
(newline)
(display " 一、开始身体症状测试 ：")(newline)
(process-question QA)
(display "初步诊断详细内容：")
(display Ctx_Attributes )
(newline)  
;风险值需要根据症状组合情况做调整：
;单纯性的乏力需要排除风险；
;乏力伴随咳嗽（干咳）或者发热，需要成倍增加风险值
;乏力伴随严重腹泻，而没有这里列举的其它症状，需要降低风险值
(let ((FA_LI (have_attribute_inResult Ctx_Attributes "乏力")))
    (if (and (not (null?  FA_LI))
             (not (equal? "无" (car (cdr FA_LI)))))
        (begin
            (display "有乏力症状，需要综合考虑其它症状计算风险...")(newline)
            (let (  (KE_SOU (have_attribute_inResult Ctx_Attributes "咳嗽"))
                    (FA_RE (have_attribute_inResult Ctx_Attributes "发热"))
                    (HU_XI (have_attribute_inResult Ctx_Attributes "呼吸困难"))
                 )
                (if (and (null? KE_SOU) (null? FA_RE) (null? HU_XI))
                    (begin
                        (display "单纯性发热，调低风险值。")(newline)
                        (set! Total_Risk (- Total_Risk 15))
                    )
                    (begin
                        ;测试是否有严重腹泻并且没有咳嗽
                        (let ((FU_XIE (have_attribute_inResult Ctx_Attributes "腹泻")))
                            (if (and    (not (null? FU_XIE)) 
                                        (equal? "明显" (car (cdr FU_XIE))) 
                                        (null? KE_SOU)
                                )
                                (set! Total_Risk 0) ;为严重腹泻引起的乏力,不会是新冠感染。
                                (begin
                                    (display "乏力伴随咳嗽或者发热，调高风险值。")(newline)
                                    (set! Total_Risk (* Total_Risk 2))
                                )
                            )
                        )
                    )
                )
            )
        )
    )
)

(newline)
(display " 二、开始进行【医院检测结果】分析 ：")(newline)
(process-question QB)
(display " 三、开始进行【流行病学调查】分析 ：")(newline)
(display "1）最近14天，您是否去过 国内重点疫区？（0-未去过，1-湖北省，2-湖北省外）")
(newline)
(set! Total_Risk (+ Total_Risk 
    (case (input_selected)
        ((0) 0)
        ((1) 50)
        ((2) 20)
        (else 0)
    )
))
(newline)
(display "2）最近14天，您是否去过 重点疫区国家？（0-未去过，1-意大利、西班牙，2-欧洲其它地方，3-美国，4-世界其它地方）")
(newline)
(set! Total_Risk (+ Total_Risk 
    (case (input_selected)
        ((0) 0)
        ((1) 50)
        ((2 4) 30)
        ((3) 80)
        (else 0)
    )
))
(newline)
(display "3）最近14天，您是否与确诊患者有过密切接触？（0-否，1-是，2-不清楚）")
(newline)
(set! Total_Risk (+ Total_Risk 
    (case (input_selected)
        ((0 2) 0)
        ((1) 50)
        (else 0)
    )
))
(newline)
(display "4）最近14天，您是否与与确诊患者密接者有接触？（0-否，1-是，2-不清楚）")
(newline)
(set! Total_Risk (+ Total_Risk 
    (case (input_selected)
        ((0 2) 0)
        ((1) 25)
        (else 0)
    )
))
(newline)
(display "5）最近14天，您是否与确诊患者同乘交通工具？（0-否，1-是，2-不清楚）")
(newline)
(set! Total_Risk (+ Total_Risk 
    (case (input_selected)
        ((0 2) 0)
        ((1) 25)
        (else 0)
    )
))
(newline)
(display "--流调分析结束--")
(newline)
(display "========================================")
(newline)
(display "您患 COVID-19 的整体风险检测概率是：")
(display Total_Risk)
(display "%")
(newline) 
(display "========================================")
(newline)
(display "Test...ok")
(newline)
;(display (get-index-value A2 1)) 
(newline)
;(display (get-index-name A2 2))
(display "输入 (exit) 离开Scheme环境")


;(input_selected )
