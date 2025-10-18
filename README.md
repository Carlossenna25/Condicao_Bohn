# Condição de Bohn — Brasil (2011–2025)

Análise da sustentabilidade fiscal brasileira com base na **Condição de Bohn (1998)**, utilizando modelo **ARDL/UECM** com dados mensais de 2011 a 2025.

## 📊 Estrutura do projeto

- **data/**: bases brutas e processadas  
- **R/**: scripts modulares de análise  
- **gráficos/**: figuras finais  
- **outputs/**: resultados numéricos e modelos estimados  

## 🧮 Modelagem

O modelo segue:
\\( s_t = \\alpha + \\beta d_{t-1} + \\gamma X_t + \\epsilon_t \\)  
com \(X_t\) = {gap, juros reais, dummies de regime}, estimado via **auto_ARDL + UECM**.

## 🧩 Principais resultados

- Violação da condição de Bohn: ∂s/∂d^LR ≈ −0.31  
- Reação fiscal pró-cíclica (gap negativo e significativo)  
- Efeito positivo das medidas de controle fiscal

## 🗂️ Fontes de dados
Banco Central do Brasil, Refinitiv e IBGE.

## 📈 Visualizações
- trajetória de dívida e resultado primário  
- crescimento relativo dívida–PIB  
- função de reação fiscal  
- efeitos de longo prazo estimados  

---

📎 **Autor:** [Carlos Sena](https://www.linkedin.com/in/carlos-sena-0776381a5/)  
📘 **Repositório:** [github.com/Carlossenna25/Condicao_Bohn](https://github.com/Carlossenna25/Condicao_Bohn)

