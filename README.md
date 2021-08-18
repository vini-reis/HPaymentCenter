# Haskell Payment Center
## Introdução

A ideia do projeto foi criar uma aplicação para realizar pagamentos de forma mais confiável utilizando a linguagem de programação Haskell, que consiste numa linguagem puramente funcional, garantindo maior confiabilidade nesse tipo de transação.

O projeto é uma aplicação web, renderizada do lado do servidor, com integração com um banco de dados local a fim de armazenar os dados de histórico e atual estado da aplicação.

## Instalação de dependências

A aplicação depende de uma instância de banco de dados do PostgreSQL instalada para que possa funcionar. Seguem as instruções para instalação

### Instalação do banco de dados (PostgreSQL)

- Para instalação do banco, pode se utilizar o script abaixo

```bash
sudo sh -c 'echo "deb http://apt.postgresql.org/pub/repos/apt $(lsb_release -cs)-pgdg main" > /etc/apt/sources.list.d/pgdg.list'
wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | sudo apt-key add -
sudo apt-get update
sudo apt-get -y install postgresql
```

- Feita a instalação, é necessário criar um usuário com as credenciais
  - Nome: `HPC`
  - Senha: `HPC`

- A criação do banco de usuários pode ser feita pelo script

```bash
sudo -u postgres psql
CREATE DATABASE hpc;
CREATE USER hpc WITH ENCRYPTED PASSWORD 'hpc';
GRANT ALL PRIVILEGES ON DATABASE hpc TO hpc;
```

- Após esses passos, seguindo as instruções de execução, a aplicação irá funcionar através do link http://localhost:3000

### Instruções de execução

- Utilizar os comandos abaixo para clonar o repositório para a máquina, e ir até o repositório

```bash
git clone git@github.com:ufabc-bcc/paradigmas-2021-q2-projeto-final-vini-reis.git
```

```bash
cd paradigmas-2021-q2-projeto-final-vini-reis.git
```

- Então, rodar o comando para compilar o projeto

```bash
stack build
```

- Para iniciar a aplicação, basta rodar o comando abaixo

```bash
stack exec -- yesod devel
```

- Com a aplicação iniciada, você poderá acessar a aplicação pelo link http://localhost:3000

### Instrução de utilização

- A aplicação inicia com dois usuários criados de exemplo, com as credenciais:
  - Usuário Admin:
    - Email: `admin@hpc.com`
    - Senha: `123123`

  - Usuário Exemplo:
    - Email: `vinicius.reis@aluno.ufabc.edu.br`
    - Senha: `11041416`

- Pode-se realizar o login com algum dos dois usuários. Ao se autenticar, é possível realizar as seguintes ações:
  - Criar novas contas, de dois tipos, até o momento, e com os status de `Ativa` ou `Inativa`
  - Pode-se alterar o status e o tipo de conta a qualquer momento, assim como adicionar saldo a sua conta
  - A fim de demonstrações, também é possível realizar transações entre as coisas, pelo botão `+`, a frente de `Transactions`
    - Vale dizer que todas as transações geram histórico para fins de registro, inclusive a adição/remoção de saldo da conta

- Também é possível criar novos usuários para utilizar a plataforma, se registrando pelo link `Register`
