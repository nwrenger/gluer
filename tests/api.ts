export interface Hello {
    name: string;
}

export async function add_root(data: Hello): Promise<Hello[]> {
    const response = await fetch("/", {
        method: "POST",
        headers: {
            "Content-Type": "application/json"
        },
        body: JSON.stringify(data)
    });
    return response.json();
}

export async function fetch_root(): Promise<string> {
    const response = await fetch("/", {
        method: "GET",
        headers: {
            "Content-Type": "application/json"
        },
        body: undefined
    });
    return response.json();
}

