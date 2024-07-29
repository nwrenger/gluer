export interface Hello {
    name: string;
}

export async function add_root(params: Hello): Promise<Hello | any> {
    const response = await fetch("/", {
        method: "POST",
        headers: {
            "Content-Type": "application/json"
        },
        body: JSON.stringify(params)
    });
    return response.json();
}

export async function fetch_root(): Promise<string | any> {
    const response = await fetch("/", {
        method: "GET",
        headers: {
            "Content-Type": "application/json"
        },
        body: undefined
    });
    return response.json();
}

