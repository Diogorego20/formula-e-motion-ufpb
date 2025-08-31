# REPRODUÇÃO ARTÍSTICA: A NOITE ESTRELADA DE VAN GOGH (VERSÃO R BASE)
# Autor: Diogo Rego - Estudante de Estatística UFPB
# Projeto: Pixel Poesia R - Arte com Linguagem de Programação
# Data: Agosto 2025
# ===============================================================================

# Esta versão usa apenas funções do R base para máxima compatibilidade

# ===============================================================================
# FUNÇÃO PRINCIPAL: CRIAR A NOITE ESTRELADA
# ===============================================================================

criar_noite_estrelada_base <- function(largura = 800, altura = 600) {
  
  # Configuração do dispositivo gráfico
  png("noite_estrelada_vangogh_base.png", width = largura, height = altura, 
      bg = "#0a0a2e", res = 150)
  
  # Configuração da área de plotagem
  par(mar = c(0, 0, 0, 0), bg = "#0a0a2e")
  plot(0, 0, type = "n", xlim = c(0, largura), ylim = c(0, altura), 
       axes = FALSE, xlab = "", ylab = "")
  
  # ===============================================================================
  # FUNDO DO CÉU COM GRADIENTE
  # ===============================================================================
  
  # Criação do gradiente do céu (do azul escuro ao azul médio)
  for (i in 1:100) {
    y_pos <- altura * 0.4 + (altura * 0.6) * (i/100)
    cor_intensidade <- 0.1 + 0.4 * (i/100)
    rect(0, y_pos, largura, y_pos + altura * 0.006, 
         col = rgb(0.1, 0.2, cor_intensidade, alpha = 0.8), border = NA)
  }
  
  # ===============================================================================
  # MONTANHAS DE FUNDO
  # ===============================================================================
  
  # Coordenadas das montanhas
  mont_x <- c(0, largura * 0.3, largura * 0.6, largura, largura, 0)
  mont_y <- c(altura * 0.4, altura * 0.5, altura * 0.45, altura * 0.48, 0, 0)
  
  polygon(mont_x, mont_y, col = "#1a1a3a", border = NA)
  
  # ===============================================================================
  # VILA COM CASAS E IGREJA
  # ===============================================================================
  
  # Casas da vila
  for (i in 1:8) {
    casa_x <- largura * (0.3 + i * 0.06)
    casa_y <- altura * 0.35
    casa_largura <- 15 + runif(1, 0, 10)
    casa_altura <- 20 + runif(1, 0, 15)
    
    # Corpo da casa
    rect(casa_x, casa_y, casa_x + casa_largura, casa_y + casa_altura, 
         col = "#2d2d4a", border = "#404060")
    
    # Telhado triangular
    telhado_x <- c(casa_x - 2, casa_x + casa_largura/2, casa_x + casa_largura + 2)
    telhado_y <- c(casa_y + casa_altura, casa_y + casa_altura + 10, casa_y + casa_altura)
    polygon(telhado_x, telhado_y, col = "#1a1a2e", border = "#404060")
    
    # Janela com luz (algumas casas)
    if (runif(1) > 0.5) {
      rect(casa_x + casa_largura/3, casa_y + casa_altura/2, 
           casa_x + 2*casa_largura/3, casa_y + 3*casa_altura/4, 
           col = "#ffeb3b", border = "#ffc107")
    }
  }
  
  # Igreja com torre
  igreja_x <- largura * 0.6
  igreja_y <- altura * 0.35
  
  # Corpo da igreja
  rect(igreja_x, igreja_y, igreja_x + 30, igreja_y + 40, 
       col = "#2d2d4a", border = "#404060")
  
  # Torre da igreja
  rect(igreja_x + 10, igreja_y + 40, igreja_x + 20, igreja_y + 80, 
       col = "#2d2d4a", border = "#404060")
  
  # ===============================================================================
  # CIPRESTE (ELEMENTO VERTICAL DOMINANTE)
  # ===============================================================================
  
  # Geração do cipreste com forma orgânica
  cipreste_x <- largura * 0.15
  cipreste_base_y <- altura * 0.1
  cipreste_topo_y <- altura * 0.95
  
  # Pontos para criar a forma do cipreste
  n_pontos <- 50
  y_pontos <- seq(cipreste_base_y, cipreste_topo_y, length.out = n_pontos)
  
  # Largura variável (mais largo na base)
  larguras <- seq(20, 4, length.out = n_pontos)
  
  # Ondulações naturais
  ondulacao <- sin(seq(0, 8*pi, length.out = n_pontos)) * 5
  
  # Lado esquerdo e direito
  x_esquerda <- cipreste_x - larguras/2 + ondulacao
  x_direita <- cipreste_x + larguras/2 + ondulacao
  
  # Desenho do cipreste
  cipreste_x_coords <- c(x_esquerda, rev(x_direita))
  cipreste_y_coords <- c(y_pontos, rev(y_pontos))
  
  polygon(cipreste_x_coords, cipreste_y_coords, col = "#0d1117", border = NA)
  
  # ===============================================================================
  # ESPIRAIS DO CÉU (CARACTERÍSTICA MARCANTE DE VAN GOGH)
  # ===============================================================================
  
  # Função para desenhar espirais
  desenhar_espiral <- function(centro_x, centro_y, raio_max, rotacao = 0, cor = "#4a90e2") {
    t <- seq(0, 4*pi, length.out = 200)
    raio <- seq(5, raio_max, length.out = length(t))
    
    x <- centro_x + raio * cos(t + rotacao)
    y <- centro_y + raio * sin(t + rotacao)
    
    # Adicionar variação orgânica
    x <- x + rnorm(length(x), 0, 2)
    y <- y + rnorm(length(y), 0, 2)
    
    # Desenhar a espiral com múltiplas linhas para dar espessura
    for (offset in c(-1, 0, 1)) {
      lines(x + offset, y + offset, col = cor, lwd = 2)
    }
  }
  
  # Desenhar múltiplas espirais
  espirais_centros <- list(
    list(x = largura * 0.3, y = altura * 0.8, raio = 60, rot = 0),
    list(x = largura * 0.5, y = altura * 0.85, raio = 80, rot = pi/3),
    list(x = largura * 0.7, y = altura * 0.75, raio = 50, rot = pi/2),
    list(x = largura * 0.4, y = altura * 0.7, raio = 40, rot = pi),
    list(x = largura * 0.6, y = altura * 0.9, raio = 35, rot = 3*pi/2),
    list(x = largura * 0.8, y = altura * 0.8, raio = 45, rot = pi/4)
  )
  
  for (espiral in espirais_centros) {
    desenhar_espiral(espiral$x, espiral$y, espiral$raio, espiral$rot, "#4a90e2")
  }
  
  # ===============================================================================
  # ESTRELAS E LUA
  # ===============================================================================
  
  # Função para desenhar estrela com halo
  desenhar_estrela <- function(x, y, tamanho = 5, cor = "#ffeb3b") {
    # Halo da estrela
    symbols(x, y, circles = tamanho * 2, add = TRUE, 
            fg = NA, bg = rgb(1, 1, 0, alpha = 0.3), inches = FALSE)
    
    # Centro brilhante da estrela
    symbols(x, y, circles = tamanho, add = TRUE, 
            fg = NA, bg = cor, inches = FALSE)
    
    # Raios da estrela
    for (angulo in seq(0, 2*pi, length.out = 8)) {
      x_fim <- x + cos(angulo) * tamanho * 3
      y_fim <- y + sin(angulo) * tamanho * 3
      lines(c(x, x_fim), c(y, y_fim), col = cor, lwd = 2)
    }
  }
  
  # Posições das estrelas
  estrelas_pos <- list(
    list(x = largura * 0.2, y = altura * 0.9, tam = 4),
    list(x = largura * 0.35, y = altura * 0.95, tam = 3),
    list(x = largura * 0.45, y = altura * 0.88, tam = 5),
    list(x = largura * 0.55, y = altura * 0.92, tam = 3),
    list(x = largura * 0.65, y = altura * 0.87, tam = 4),
    list(x = largura * 0.75, y = altura * 0.93, tam = 3),
    list(x = largura * 0.82, y = altura * 0.85, tam = 4),
    list(x = largura * 0.9, y = altura * 0.9, tam = 3),
    list(x = largura * 0.25, y = altura * 0.82, tam = 3),
    list(x = largura * 0.72, y = altura * 0.78, tam = 3),
    list(x = largura * 0.88, y = altura * 0.82, tam = 4)
  )
  
  # Desenhar todas as estrelas
  for (estrela in estrelas_pos) {
    desenhar_estrela(estrela$x, estrela$y, estrela$tam)
  }
  
  # Lua (estrela maior)
  lua_x <- largura * 0.85
  lua_y <- altura * 0.8
  
  # Halo da lua
  symbols(lua_x, lua_y, circles = 25, add = TRUE, 
          fg = NA, bg = rgb(1, 1, 0, alpha = 0.4), inches = FALSE)
  
  # Corpo da lua
  symbols(lua_x, lua_y, circles = 15, add = TRUE, 
          fg = NA, bg = "#ffeb3b", inches = FALSE)
  
  # Raios da lua
  for (angulo in seq(0, 2*pi, length.out = 12)) {
    x_fim <- lua_x + cos(angulo) * 30
    y_fim <- lua_y + sin(angulo) * 30
    lines(c(lua_x, lua_x + cos(angulo) * 20), 
          c(lua_y, lua_y + sin(angulo) * 20), 
          col = "#ffeb3b", lwd = 3)
  }
  
  # ===============================================================================
  # FINALIZAÇÃO
  # ===============================================================================
  
  # Adicionar assinatura artística
  text(largura * 0.02, altura * 0.02, 
       "Reprodução Digital: Diogo Rego (UFPB) - Pixel Poesia R", 
       col = "#ffffff", cex = 0.6, adj = 0)
  
  text(largura * 0.02, altura * 0.05, 
       "Obra Original: A Noite Estrelada - Vincent van Gogh (1889)", 
       col = "#cccccc", cex = 0.5, adj = 0)
  
  # Fechar o dispositivo gráfico
  dev.off()
  
  cat("=== REPRODUÇÃO ARTÍSTICA CONCLUÍDA ===\n")
  cat("Arquivo salvo: noite_estrelada_vangogh_base.png\n")
  cat("Dimensões:", largura, "x", altura, "pixels\n")
  cat("Técnica: Programação artística em R (base)\n")
  cat("Autor: Diogo Rego - Estudante de Estatística UFPB\n")
  cat("=======================================\n")
}

# =================================================