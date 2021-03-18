#====Plotting Functions====
plotDailySummary <- function(data, colors, plotVariable, plotType, dates, addPoints, stderr){
  if (plotType == "line"){
    plot <- ggplot(data, aes(x=variable, y=mean, color=myaxis, group=myaxis)) +
      geom_point() +
      geom_line() +
      scale_color_manual(values=colors) +
      theme_bw(base_size = 16) +
      labs(y = plotVariable, x = NULL, group = NULL, color = NULL) +
      scale_x_discrete(breaks=unique(data$variable), labels=dates)  +
      theme(aspect.ratio = 0.5)
    
    if (stderr == "Yes"){
      plot <- plot +
        geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.1)
    }
  } else {
    plot <- ggplot(data, aes(x=variable, y=value, fill=myaxis))
    if (plotType == "box"){
      plot <- plot + 
        geom_boxplot(varwidth = TRUE, alpha=0.6)
    } else if (plotType == "violin"){
      plot <- plot +
        geom_violin(alpha=0.6)
    } else if (plotType == "bar"){
      plot <- plot +
        geom_bar(stat = "summary", colour="black", position = "dodge", alpha=0.6)
      if (stderr == "Yes"){
        plot <- plot +
          geom_errorbar( aes(x=variable, ymin=mean-se, ymax=mean+se), 
                         colour="black", width = 0.4, position=position_dodge(.9))
      }
    }
    plot <- plot +
      scale_fill_manual(values=colors) +
      theme_bw(base_size = 16) +
      labs(y = plotVariable, x = NULL, fill = NULL) +
      scale_x_discrete(breaks=unique(data$variable), labels=dates)  +
      theme(aspect.ratio = 0.5)
    
    if (addPoints == "Yes"){
      plot <- plot +
        geom_jitter(aes(color = myaxis), position = position_jitterdodge(),
                    show.legend = FALSE) +
        scale_color_manual(values=colors)
    }
    
  }
  plot
}
plotAveSleep <- function(data, colors, stderr){
  plot <- ggplot(data, aes(x=Hour, y=mean, color=myaxis, group=myaxis)) +
    geom_point() +
    geom_line() +
    scale_color_manual(values=colors) +
    theme_bw(base_size = 16) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  +
    scale_x_continuous(breaks=c(0,3,6,9,12,15,18,21)) +
    theme(aspect.ratio = 0.5)
  
  if (stderr == "Yes"){
    plot <- plot +
      labs(x = "Hour [ZT]", y = "Mean Sleep (minutes) [SE]", group = NULL, color = NULL) +
      geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.1)
  } else {
    plot <- plot +
      labs(x = "Hour [ZT]", y = "Mean Sleep (minutes)", group = NULL, color = NULL)
  }
  plot
}
plotAveActivity <- function(data, colors, stderr){
  plot <- ggplot(data, aes(x=Hour, y=mean, color=myaxis, group=myaxis)) +
    geom_point() +
    geom_line() +
    scale_color_manual(values=colors) +
    theme_bw(base_size = 16) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  +
    scale_x_continuous(breaks=c(0,3,6,9,12,15,18,21)) +
    theme(aspect.ratio = 0.5)
  
  if (stderr == "Yes"){
    plot <- plot +
      labs(x = "Hour [ZT]", y = "Mean Activity (beam breaks) [SE]", group = NULL, color = NULL) +
      geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.1)
  } else {
    plot <- plot +
      labs(x = "Hour [ZT]", y = "Mean Activity (beam breaks)", group = NULL, color = NULL)
  }
  plot
}
plotAverageSummary <- function(data, colors, plotType, addPoints, stderr){
  if (plotType == "density"){
    plot <- ggplot(data, aes(x = value, group = Condition.Genotype, fill = Condition.Genotype)) +
      geom_density(adjust=1.5, alpha=.4) +
      scale_fill_manual(values=colors) +
      theme_bw(base_size = 16) +
      labs(x = data$variable, fill = NULL) +
      theme(aspect.ratio = 0.5)
    
    plot <- plot +
      geom_vline(aes(xintercept=mean, color=Condition.Genotype),
                 linetype="dashed", show.legend = FALSE)
  } else {
    plot <- ggplot(data, aes(x = myaxis, y = value, fill = Condition.Genotype))
    if (plotType == "box"){
      plot <- plot +
        geom_boxplot(varwidth = TRUE, alpha=0.6)
    } else if (plotType == "violin"){
      plot <- plot +
        geom_violin(alpha=0.6)
    } else if (plotType == "bar"){
      plot <- plot +
        geom_bar(stat = "summary", colour="black", alpha=0.6)
      if (stderr == "Yes"){
        plot <- plot +
          geom_errorbar( aes(x=myaxis, ymin=mean-se, ymax=mean+se), colour="black", width = 0.4)
      }
    }
    plot <- plot +
      scale_fill_manual(values=colors) +
      theme_bw(base_size = 16) +
      labs(y = data$variable, x = NULL, fill = NULL) +
      theme(aspect.ratio = 1)
    
    if (addPoints == "Yes"){
      plot <- plot + 
        geom_jitter(aes(color = myaxis), show.legend = FALSE) +
        scale_color_manual(values=colors)
    }
  }
  plot
}
plotMortality <- function(data, colors, names){
  plot <- survminer::ggsurvplot(
    survfit(Surv(days, dead) ~ Condition.Genotype, data),
    data = data,
    pval = TRUE,
    conf.int = TRUE,
    palette = colors,
    xlab = "Time in days",
    risk.table = "abs_pct",
    risk.table.y.txt.col = TRUE,
    risk.table.y.txt = FALSE,
    surv.median.line = "hv",
    legend.labs = names
  )  
  plot$plot <- plot$plot + theme(aspect.ratio = 0.25)
  plot$table <- plot$table + theme(aspect.ratio = 0.05)
  plot
}