# Demo 3 – Gateway, Control e Host

Este protótipo foi criado para demonstrar uma topologia Gateway/Control/Host com roteamento seguro entre canais de controle e dados. O objetivo é validar cenários de carga onde Shell, Câmera e Gerenciador de Arquivos operam simultaneamente. No Demo 3, os painéis de usuários do Control e do Gateway usam `TVirtualStringTree` com colunas para Nome de Usuário, ID do Cliente, IP, Tipo (Host/Admin), Latência, Status, GUID e HWID.

## Revisão de riscos encontrados
- **Roteamento (Gateway)**: separar canais por cabeçalhos explícitos (`TGatewayChannel`) e reforçar checagem de tamanho máximo para evitar que um frame de vídeo seja entregue como comando de shell.
- **Thread-safety de listas**: `TThreadSafeClientList` encapsula uma `TRTLCriticalSection` para operações de adicionar/remover/buscar clientes evitando condições de corrida quando múltiplos hosts entram ou saem.
- **Liberação de Streams**: uso de `try/finally` em todos os pontos que recebem `TStream` (vídeo, arquivo, shell) garantindo `Free` após a redistribuição.
- **Bloqueios de IO**: upload pesado é enviado via `TTask`/fila assíncrona e o heartbeat é publicado em `TTimer` separado, evitando que o ping dependa do socket de dados do arquivo.

## Organização
- `Common/PortalTypes.pas`: contratos, enums de canal e estruturas compartilhadas.
- `Common/ThreadSafeLists.pas`: lista segura para os clientes ativos.
- `Gateway/*`: aplicativo VCL simples para roteamento e monitoramento.
- `Control/*`: cliente administrador com tree de hosts e abas Shell/Câmera/Arquivos.
- `Host/*`: agente headless que registra componentes e aguarda comandos.

## Roteiro de teste de carga simultânea
1. **Preparação**
   - Levante o `Gateway.exe` na porta definida (padrão 8090) e valide logs de listeners.
   - Inicie `Host.exe` com webcam simulada (timer que gera frames) e habilite o file uploader.
   - Conecte `Control.exe` ao Gateway e selecione o host na árvore.
2. **Cenário combinado**
   - Aba **Câmera**: inicie streaming (frames de ~256KB) e confirme que o canal `gcCamera` mantém taxa de quadros estável.
   - Aba **Arquivos**: envie um arquivo ≥200 MB. O upload deve usar fila assíncrona (`BeginFileUpload`) mantendo heartbeat ativo.
   - Aba **Shell**: execute comandos simples (`dir`, `ping 127.0.0.1`) e observe resposta em < 500 ms.
3. **Monitoração**
   - Verifique no Gateway se `LatencyMs` permanece dentro do SLA (< 150 ms) e se o status do host não oscila durante o upload.
   - Monitore consumo de memória do Gateway para garantir que os buffers de vídeo/arquivo sejam liberados (valor estável após término).
4. **Critérios de sucesso**
   - Nenhuma desconexão falsa de heartbeat durante upload pesado.
   - Nenhum pacote de vídeo cai no canal de shell (logs de roteamento sem erros de cabeçalho).
   - Após concluir o upload, GC de buffers mantém uso de memória próximo ao baseline (+5% máximo).
