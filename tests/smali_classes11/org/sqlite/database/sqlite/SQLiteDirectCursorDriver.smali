.class public final Lorg/sqlite/database/sqlite/SQLiteDirectCursorDriver;
.super Ljava/lang/Object;

# interfaces
.implements Lorg/sqlite/database/sqlite/SQLiteCursorDriver;


# static fields
.field static iaEtvw:Ljava/lang/String;
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Lint;"
        }
    .end annotation
.end field


# instance fields
.field private EQVAdg:Lorg/sqlite/database/sqlite/SQLiteQuery;

.field private final QAcdVn:Landroid/os/CancellationSignal;

.field private final ThTaYB:Ljava/lang/String;

.field private final YjPJqf:Lorg/sqlite/database/sqlite/SQLiteDatabase;

.field private final xBlCIA:Ljava/lang/String;


# direct methods
.method static constructor <clinit>()V
    .locals 1

    const/4 v0, 0x0

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteDirectCursorDriver;->Pbq(Z)V

    return-void
.end method

.method public constructor <init>(Lorg/sqlite/database/sqlite/SQLiteDatabase;Ljava/lang/String;Ljava/lang/String;Landroid/os/CancellationSignal;)V
    .locals 0

    invoke-direct {p0}, Ljava/lang/Object;-><init>()V

    iput-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteDirectCursorDriver;->YjPJqf:Lorg/sqlite/database/sqlite/SQLiteDatabase;

    iput-object p3, p0, Lorg/sqlite/database/sqlite/SQLiteDirectCursorDriver;->ThTaYB:Ljava/lang/String;

    iput-object p2, p0, Lorg/sqlite/database/sqlite/SQLiteDirectCursorDriver;->xBlCIA:Ljava/lang/String;

    iput-object p4, p0, Lorg/sqlite/database/sqlite/SQLiteDirectCursorDriver;->QAcdVn:Landroid/os/CancellationSignal;

    return-void
.end method

.method public static synthetic Pbq(Z)V
    .locals 0
    .annotation system Ldalvik/annotation/MethodParameters;
        accessFlags = {
            0x1000,
            0x1000
        }
        names = {
            "#",
            "#"
        }
    .end annotation

    .annotation system Ldalvik/annotation/Signature;
        value = {
            "(",
            "Lc;",
            "Lu;",
            "Ln;",
            ")",
            "Lt;"
        }
    .end annotation

    if-eqz p0, :cond_0

    const/4 p0, 0x0

    invoke-static {p0}, Lorg/sqlite/database/sqlite/SQLiteDirectCursorDriver;->Pbq(Z)V

    :cond_0
    const-string p0, "HZe@XNjDZziTaRTV_Uv[UM[O\u0002\u000f"

    invoke-static {p0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    sput-object p0, Lorg/sqlite/database/sqlite/SQLiteDirectCursorDriver;->iaEtvw:Ljava/lang/String;

    return-void
.end method


# virtual methods
.method public cursorClosed()V
    .locals 0

    return-void
.end method

.method public cursorDeactivated()V
    .locals 0

    return-void
.end method

.method public cursorRequeried(Landroid/database/Cursor;)V
    .locals 0

    return-void
.end method

.method public query(Lorg/sqlite/database/sqlite/SQLiteDatabase$CursorFactory;[Ljava/lang/String;)Landroid/database/Cursor;
    .locals 4

    new-instance v0, Lorg/sqlite/database/sqlite/SQLiteQuery;

    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteDirectCursorDriver;->YjPJqf:Lorg/sqlite/database/sqlite/SQLiteDatabase;

    iget-object v2, p0, Lorg/sqlite/database/sqlite/SQLiteDirectCursorDriver;->xBlCIA:Ljava/lang/String;

    iget-object v3, p0, Lorg/sqlite/database/sqlite/SQLiteDirectCursorDriver;->QAcdVn:Landroid/os/CancellationSignal;

    invoke-direct {v0, v1, v2, v3}, Lorg/sqlite/database/sqlite/SQLiteQuery;-><init>(Lorg/sqlite/database/sqlite/SQLiteDatabase;Ljava/lang/String;Landroid/os/CancellationSignal;)V

    :try_start_0
    invoke-virtual {v0, p2}, Lorg/sqlite/database/sqlite/SQLiteQuery;->bindAllArgsAsStrings([Ljava/lang/String;)V

    if-nez p1, :cond_0

    new-instance p1, Lorg/sqlite/database/sqlite/SQLiteCursor;

    iget-object p2, p0, Lorg/sqlite/database/sqlite/SQLiteDirectCursorDriver;->ThTaYB:Ljava/lang/String;

    invoke-direct {p1, p0, p2, v0}, Lorg/sqlite/database/sqlite/SQLiteCursor;-><init>(Lorg/sqlite/database/sqlite/SQLiteCursorDriver;Ljava/lang/String;Lorg/sqlite/database/sqlite/SQLiteQuery;)V

    goto :goto_0

    :cond_0
    iget-object p2, p0, Lorg/sqlite/database/sqlite/SQLiteDirectCursorDriver;->YjPJqf:Lorg/sqlite/database/sqlite/SQLiteDatabase;

    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteDirectCursorDriver;->ThTaYB:Ljava/lang/String;

    invoke-interface {p1, p2, p0, v1, v0}, Lorg/sqlite/database/sqlite/SQLiteDatabase$CursorFactory;->newCursor(Lorg/sqlite/database/sqlite/SQLiteDatabase;Lorg/sqlite/database/sqlite/SQLiteCursorDriver;Ljava/lang/String;Lorg/sqlite/database/sqlite/SQLiteQuery;)Landroid/database/Cursor;

    move-result-object p1
    :try_end_0
    .catch Ljava/lang/RuntimeException; {:try_start_0 .. :try_end_0} :catch_0

    :goto_0
    iput-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteDirectCursorDriver;->EQVAdg:Lorg/sqlite/database/sqlite/SQLiteQuery;

    return-object p1

    :catch_0
    move-exception p1

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteQuery;->close()V

    throw p1
.end method

.method public setBindArguments([Ljava/lang/String;)V
    .locals 1

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteDirectCursorDriver;->EQVAdg:Lorg/sqlite/database/sqlite/SQLiteQuery;

    invoke-virtual {v0, p1}, Lorg/sqlite/database/sqlite/SQLiteQuery;->bindAllArgsAsStrings([Ljava/lang/String;)V

    return-void
.end method

.method public toString()Ljava/lang/String;
    .locals 2

    new-instance v0, Ljava/lang/StringBuilder;

    invoke-direct {v0}, Ljava/lang/StringBuilder;-><init>()V

    sget-object v1, Lorg/sqlite/database/sqlite/SQLiteDirectCursorDriver;->iaEtvw:Ljava/lang/String;

    invoke-virtual {v0, v1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteDirectCursorDriver;->xBlCIA:Ljava/lang/String;

    invoke-virtual {v0, v1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    invoke-virtual {v0}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v0

    return-object v0
.end method
