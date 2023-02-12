package io.github.jmurmel;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.OutputStream;
import java.io.StringWriter;
import java.net.InetSocketAddress;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import javax.script.*;
import com.sun.net.httpserver.*;

/** a simple Webserver that will dynamically serve Murmel files.
 *  Usage:
 *  java -cp jmurmel.jar MurmelHttpServer.java [murmelfiledir] */
public final class MurmelHttpServer {

    private MurmelHttpServer() {}

    public static void main(String[] args) throws Exception {
        final Path murmelDir = args.length == 1 ? Paths.get(args[0]).toAbsolutePath() : Paths.get("www").toAbsolutePath();

        if (!Files.isDirectory(murmelDir)) {
            System.err.println(murmelDir + " does not exist or is not a directory, aborting.");
            System.exit(1);
        }
        if (!Files.isReadable(murmelDir)) {
            System.err.println(murmelDir + " is not readable, aborting.");
            System.exit(1);
        }

        System.out.println("Serving " + murmelDir + " on port 8080/murmel/, press CTRL-C to end");

        final HttpServer server = HttpServer.create(new InetSocketAddress(8080), 0);

        server.createContext("/ping", t -> writeResponse(t, 200, "pong"));
        server.createContext("/murmel/", new MurmelHandler(murmelDir));

        server.setExecutor(null); // creates a default executor
        server.start();
    }

    static void writeResponse(HttpExchange t, int httpCode, String msg) throws IOException {
        try (OutputStream os = t.getResponseBody()) {
            t.sendResponseHeaders(httpCode, msg.length());
            os.write(msg.getBytes(java.nio.charset.StandardCharsets.ISO_8859_1));
        }
    }

    static class MurmelHandler implements HttpHandler {
        private final Path murmelDir;
        private final ScriptEngineManager manager = new ScriptEngineManager();

        MurmelHandler(Path murmelDir) {
            this.murmelDir = murmelDir;
        }

        @Override
        public void handle(HttpExchange t) throws IOException {
            final String requestUri = t.getRequestURI().getPath();
            System.out.println("URI: " + requestUri);

            final Path filePath;
            try { filePath = murmelDir.resolve(requestUri.substring("/murmel/".length())).normalize(); }
            catch (Exception e) { writeResponse(t, 400, "Bad request URI '" + requestUri + "'"); return; }
            System.out.println("file path: " + filePath);

            if (!Files.isRegularFile(filePath) || !Files.isReadable(filePath)) {
                writeResponse(t, 404, "File " + t.getRequestURI() + " not found");
                return;
            }

            final ScriptContext ctx = new SimpleScriptContext();
            prepareMurmelEnv(ctx, t);

            try (BufferedReader reader = Files.newBufferedReader(filePath)) {
                final ScriptEngine engine = manager.getEngineByName("jmurmel");
                if (engine == null) { writeResponse(t, 500, "Configuration error: cannot create a Murmel interpreter");  return; }
                final StringWriter response = new StringWriter();
                ctx.setWriter(response);
                engine.eval(reader, ctx);
                writeResponse(t, 200, response.toString());
            }
            catch (Exception e) {
                writeResponse(t, 500, "Error: " + e.getMessage());
            }
        }

        private static void prepareMurmelEnv(ScriptContext ctx, HttpExchange t) {
            ctx.setAttribute("*request-uri*", t.getRequestURI().toASCIIString(), ScriptContext.ENGINE_SCOPE);
            ctx.setAttribute("*query-string*", t.getRequestURI().getRawQuery(), ScriptContext.ENGINE_SCOPE);
            ctx.setAttribute("*remote-address*", t.getRemoteAddress().getHostString(), ScriptContext.ENGINE_SCOPE);
            ctx.setAttribute("*request-method*", t.getRequestMethod(), ScriptContext.ENGINE_SCOPE);
            ctx.setAttribute("*request-headers*", t.getRequestHeaders(), ScriptContext.ENGINE_SCOPE);
            ctx.setAttribute("*response-headers*", t.getResponseHeaders(), ScriptContext.ENGINE_SCOPE);
        }
    }
}