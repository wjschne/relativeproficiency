library(shiny)
library(ggplot2)
library(tibble)
library(dplyr)
library(ggtext)
library(scales)
library(purrr)
options(shiny.useragg = TRUE)
rpi <- function(x,
         mu = 500,
         scale = 20 / log(9),
         criterion = .9,
         reverse = FALSE,
         interpretation = FALSE) {
    if (criterion >= 1 | criterion <= 0) stop("criterion must be between 0 and 1, exclusive")
    if (reverse) {
        r <- (1 + exp(log((1 - criterion) / criterion) + (x - mu) / scale )) ^ -1
    } else {
        r <- (1 + exp(-(log(criterion / (1 - criterion)) + (x - mu) / scale))) ^ -1
    }
    
    class(r) <- c("rpi", class(r))
    attr(r, "criterion") <- criterion
    attr(r, "reverse") <- reverse
    attr(r, "interpretation") <- interpretation
    attr(r, "scale") <- scale
    r
    
}
# tinter::darken(viridis::viridis(3, begin = .2, end = .75), .2) %>% scales::show_col()

my_colors <- c("#34376D", "#1C6F72", "#4BA250")

w <- seq(400, 600)
coef_w <- 20 / log(9)
z <- (w - 500) / coef_w

angle2hjust <- function(theta, multiplier = 1.5, as_degrees = FALSE) {
    if (as_degrees) 
        theta <- theta * pi/180
    (((cos(theta + pi) + 1)/2) - 0.5) * multiplier + 0.5
}

angle2vjust <- function(theta, multiplier = 1.5, as_degrees = FALSE) {
    if (as_degrees) 
        theta <- theta * pi/180
    (((sin(theta + pi) + 1)/2) - 0.5) * multiplier + 0.5
}

w2p <- function(w, difficulty) {
    (1 + 9 ^ ((difficulty - w) / 20))^(-1)
}

prob_label <- function(p, accuracy = 0.01, digits = NULL, max_digits = NULL, 
          remove_leading_zero = TRUE, round_zero_one = TRUE) 
{
    if (is.null(digits)) {
        l <- scales::number(p, accuracy = accuracy)
    }
    else {
        sig_digits <- abs(ceiling(log10(p + p/1e+09)) - digits)
        pgt99 <- p > 0.99
        sig_digits[pgt99] <- abs(ceiling(log10(1 - p[pgt99])) - 
                                     digits + 1)
        sig_digits[ceiling(log10(p)) == log10(p) & (-log10(p) >= 
                                                        digits)] <- sig_digits[ceiling(log10(p)) == log10(p) & 
                                                                                   (-log10(p) >= digits)] - 1
        sig_digits[is.infinite(sig_digits)] <- 0
        l <- purrr::map2_chr(p, sig_digits, formatC, format = "f", 
                             flag = "#")
    }
    if (remove_leading_zero) 
        l <- sub("^-0", "-", sub("^0", "", l))
    if (round_zero_one) {
        l[p == 0] <- "0"
        l[p == 1] <- "1"
        l[p == -1] <- "-1"
    }
    if (!is.null(max_digits)) {
        if (round_zero_one) {
            l[round(p, digits = max_digits) == 0] <- "0"
            l[round(p, digits = max_digits) == 1] <- "1"
            l[round(p, digits = max_digits) == -1] <- "-1"
        }
        else {
            l[round(p, digits = max_digits) == 0] <- paste0(".", 
                                                            paste0(rep("0", max_digits), collapse = ""))
            l[round(p, digits = max_digits) == 1] <- paste0("1.", 
                                                            paste0(rep("0", max_digits), collapse = ""))
            l[round(p, digits = max_digits) == -1] <- paste0("-1.", 
                                                             paste0(rep("0", max_digits), collapse = ""))
        }
    }
    l <- sub(pattern = "-", replacement = "−", x = l)
    Encoding(l) <- "UTF-8"
    dim(l) <- dim(p)
    l
}


ui <-
    fluidPage(
        titlePanel("Generalized Relative Proficiency"),
        sidebarLayout(
            sidebarPanel(
            tabsetPanel(id = "criterion_type",
                tabPanel("Peer", value = "peer",
            sliderInput(
                "criterion_peer",
                "Success Probability for Typical Peer",
                min = 0,
                max = 1,
                value = .90,
                step = .01
            )),
            tabPanel("Student", value = "student",
                     sliderInput(
                         "criterion_student",
                         "Success Probability for Student",
                         min = 0,
                         max = 1,
                         value = .90,
                         step = .01
                     ))),
                         sliderInput(
                             "w",
                             "Student Ability (W)",
                             min = 400,
                             max = 600,
                             value = 540,
                             step = 1
                         ),
                         sliderInput(
                             "refw",
                             "Average Peer Ability (Reference W)",
                             min = 400,
                             max = 600,
                             value = 500,
                             step = 1
                         
            )
            ),
            mainPanel = mainPanel(plotOutput("modelplot"))
        )
    )
server <- function(input, output, session) {
    output$modelplot <- renderPlot({
        
        logit_student <- log(input$criterion_student / (1 - input$criterion_student))
        bln_selector <- input$criterion_type == "peer" 
        mu <- (input$refw - 500) / coef_w
        z_student <- (input$w - 500) / coef_w
        z_student1 <- (input$w - 500 + 1) / coef_w
        
        if (input$criterion_type == "peer") {
            logit <- log(input$criterion_peer / (1 - input$criterion_peer))
            p_peer <- rpi(input$w, mu = input$refw, criterion = input$criterion_peer)
            rpi_numerator <- as.numeric(prob_label(rpi(input$w), digits = 2)) * 100
            rpi_denominator <- round(input$criterion_peer * 100)
            p_peer <- input$criterion_peer
            difficulty <- mu - logit
            difficulty_w <- difficulty * 20 / log(9) + 500
            p <- rpi(w, mu = input$refw, criterion = input$criterion_peer)
            p_student <- (1 + exp(-(z_student - mu) - logit)) ^ (-1)
            p_student_theta <- atan(((1 + exp(-(z_student1 - mu) - logit)) ^ (-1) - p_student) * 200) - pi / 2
            subtitlemessage <- paste0("When peers of average ability encounter items they can answer correctly ",input$criterion_peer * 100,"% of the time, this student has a ",rpi_numerator,"% chance of answering them correctly.")
        } else {
            logit <- log(input$criterion_student / (1 - input$criterion_student))
            p_peer <- rpi(input$w, mu = input$refw, criterion = input$criterion_student, reverse = T)
            rpi_numerator <- round(input$criterion_student * 100)
            rpi_denominator <- as.numeric(prob_label(p_peer, digits = 2)) * 100
            
            difficulty_diff <- input$w - (logit * 20 / log(9) + 500)
            
            p_student <- input$criterion_student
 
        
            
            p <- (1 + 9^((difficulty_diff + 500 - w)/(20))) ^ (-1)
            
            difficulty_w <- input$w - logit * 20 / log(9)
            subtitlemessage <- paste0("When this student encounters items the student can answer correctly ",input$criterion_student * 100,"% of the time, peers of average ability have a ",rpi_denominator,"% chance of answering them correctly.")

        }
        
        
        
        


        
        
        d <- tibble(w = w, p = p) 
        ggplot(d, aes(w, p)) + 
            annotate("rect", xmin = 390, xmax = 610, ymin = 1.01, ymax = 1.13, fill = "white", color = NA) +
            geom_line(color = "dodgerblue3", linewidth = 1) + 
            geom_vline(xintercept = input$refw, color = my_colors[1], alpha = .5, linewidth = 1) +
            geom_vline(xintercept = input$w, color = my_colors[2], alpha = .5, linewidth = 1) +
            geom_vline(xintercept = difficulty_w, color = my_colors[3], alpha = .5, linewidth = 1) +
            geom_hline(yintercept = input$criterion, color = my_colors[1], alpha = .5) +
            geom_richtext(d = tibble(w = input$refw, p = 1), vjust = -1.1, label.color = NA, aes(label = paste0("Peer W = ", input$refw)), color = my_colors[1], size = 5) +
            geom_richtext(d = tibble(w = input$w, p = 1), vjust = -2.1, label.color = NA, aes(label = paste0("Student W = ", input$w)), color = my_colors[2], size = 5) +
            annotate("point", 
                     x = input$w, 
                     y = p_student,
                     size = 2,
                     color = my_colors[1]) +
            annotate("point", 
                     x = input$refw, 
                     y = p_peer,
                     size = 2,
                     color = my_colors[2]) +
            annotate("richtext", 
                     x = input$w, 
                     y = p_student,
                     label = paste0("Student ", prob_label(p_student, digits = 2)),
                     size = 5,
                     label.color = NA,
                     label.margin = unit(3, "pt"),
                     label.padding = unit(2, "pt"),
                     hjust = 0,
                     vjust = 1,
                     color = my_colors[2]) +
            annotate("richtext", 
                     x = input$refw, 
                     y = p_peer,
                     label = paste0("Peer ", prob_label(p_peer, digits = 2)),
                     size = 5,
                     label.color = NA,
                     label.margin = unit(3, "pt"),
                     label.padding = unit(2, "pt"),
                     hjust = 1,
                     vjust = 0,
                     color = my_colors[1]) +
            annotate("richtext",
                     x = difficulty_w,
                     y = 1,
                     vjust = 0, 
                     size = 5,
                     label.padding = margin(2,2,2,2, "pt"),
                     label.margin = margin(4,4,4,4, "pt"),
                     label.color = NA,
                     color = my_colors[3],
                     label = paste0("Item Difficulty = ", round(difficulty_w))) +
            scale_x_continuous("Ability and Item Difficulty (W Scores)",breaks = seq(400, 600, 20), expand = expansion()) +
            scale_y_continuous("Probability of Success",breaks = seq(0, 1, .2), labels = prob_label, expand = expansion(add = c(.0,0))) +
            theme_minimal(base_size = 18) + 
            coord_fixed(200, xlim = c(390,610)) + 
            ggtitle(paste0("Generalized Relative Proficiency = ", rpi_numerator, "/", rpi_denominator),
                    subtitle = subtitlemessage) + 
            theme(plot.subtitle = element_textbox_simple(size = 16, margin = margin(7, 0, 20, 0) ))
        
    }, width = 700, height = 700)
}

shinyApp(ui, server)
