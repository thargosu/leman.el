//! Leman E2EE agent binary: reads JSON lines from stdin, writes
//! responses to stdout, exits on `quit` or EOF.

use std::io::BufRead;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let mut agent = leman_agent::Agent::new();
    let stdin = std::io::stdin();
    for line in stdin.lock().lines() {
        let line = line?;
        match agent.handle_line(&line).await? {
            leman_agent::Flow::Quit(response) => {
                println!("{response}");
                break;
            }
            leman_agent::Flow::Respond(response) if response.is_empty() => continue,
            leman_agent::Flow::Respond(response) => println!("{response}"),
        }
    }
    Ok(())
}
