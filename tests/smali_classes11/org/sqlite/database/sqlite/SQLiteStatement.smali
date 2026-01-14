.class public final Lorg/sqlite/database/sqlite/SQLiteStatement;
.super Lorg/sqlite/database/sqlite/SQLiteProgram;


# static fields
.field static EUTgsl:Ljava/lang/String;
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Lint;"
        }
    .end annotation
.end field


# direct methods
.method static constructor <clinit>()V
    .locals 1

    const/4 v0, 0x0

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->FEm(Z)V

    return-void
.end method

.method constructor <init>(Lorg/sqlite/database/sqlite/SQLiteDatabase;Ljava/lang/String;[Ljava/lang/Object;)V
    .locals 1

    const/4 v0, 0x0

    invoke-direct {p0, p1, p2, p3, v0}, Lorg/sqlite/database/sqlite/SQLiteProgram;-><init>(Lorg/sqlite/database/sqlite/SQLiteDatabase;Ljava/lang/String;[Ljava/lang/Object;Landroid/os/CancellationSignal;)V

    return-void
.end method

.method public static synthetic FEm(Z)V
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

    invoke-static {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->FEm(Z)V

    :cond_0
    const-string p0, "HZe@XN~_GxxAO\u001d\u0006"

    invoke-static {p0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    sput-object p0, Lorg/sqlite/database/sqlite/SQLiteStatement;->EUTgsl:Ljava/lang/String;

    return-void
.end method


# virtual methods
.method public execute()V
    .locals 5

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->acquireReference()V

    :try_start_0
    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->getSession()Lorg/sqlite/database/sqlite/SQLiteSession;

    move-result-object v0

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->getSql()Ljava/lang/String;

    move-result-object v1

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->getBindArgs()[Ljava/lang/Object;

    move-result-object v2

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->getConnectionFlags()I

    move-result v3

    const/4 v4, 0x0

    invoke-virtual {v0, v1, v2, v3, v4}, Lorg/sqlite/database/sqlite/SQLiteSession;->execute(Ljava/lang/String;[Ljava/lang/Object;ILandroid/os/CancellationSignal;)V
    :try_end_0
    .catch Lorg/sqlite/database/sqlite/SQLiteDatabaseCorruptException; {:try_start_0 .. :try_end_0} :catch_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->releaseReference()V

    return-void

    :catchall_0
    move-exception v0

    goto :goto_0

    :catch_0
    move-exception v0

    :try_start_1
    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->onCorruption()V

    throw v0
    :try_end_1
    .catchall {:try_start_1 .. :try_end_1} :catchall_0

    :goto_0
    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->releaseReference()V

    throw v0
.end method

.method public executeInsert()J
    .locals 5

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->acquireReference()V

    :try_start_0
    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->getSession()Lorg/sqlite/database/sqlite/SQLiteSession;

    move-result-object v0

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->getSql()Ljava/lang/String;

    move-result-object v1

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->getBindArgs()[Ljava/lang/Object;

    move-result-object v2

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->getConnectionFlags()I

    move-result v3

    const/4 v4, 0x0

    invoke-virtual {v0, v1, v2, v3, v4}, Lorg/sqlite/database/sqlite/SQLiteSession;->executeForLastInsertedRowId(Ljava/lang/String;[Ljava/lang/Object;ILandroid/os/CancellationSignal;)J

    move-result-wide v0
    :try_end_0
    .catch Lorg/sqlite/database/sqlite/SQLiteDatabaseCorruptException; {:try_start_0 .. :try_end_0} :catch_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->releaseReference()V

    return-wide v0

    :catchall_0
    move-exception v0

    goto :goto_0

    :catch_0
    move-exception v0

    :try_start_1
    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->onCorruption()V

    throw v0
    :try_end_1
    .catchall {:try_start_1 .. :try_end_1} :catchall_0

    :goto_0
    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->releaseReference()V

    throw v0
.end method

.method public executeUpdateDelete()I
    .locals 5

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->acquireReference()V

    :try_start_0
    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->getSession()Lorg/sqlite/database/sqlite/SQLiteSession;

    move-result-object v0

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->getSql()Ljava/lang/String;

    move-result-object v1

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->getBindArgs()[Ljava/lang/Object;

    move-result-object v2

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->getConnectionFlags()I

    move-result v3

    const/4 v4, 0x0

    invoke-virtual {v0, v1, v2, v3, v4}, Lorg/sqlite/database/sqlite/SQLiteSession;->executeForChangedRowCount(Ljava/lang/String;[Ljava/lang/Object;ILandroid/os/CancellationSignal;)I

    move-result v0
    :try_end_0
    .catch Lorg/sqlite/database/sqlite/SQLiteDatabaseCorruptException; {:try_start_0 .. :try_end_0} :catch_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->releaseReference()V

    return v0

    :catchall_0
    move-exception v0

    goto :goto_0

    :catch_0
    move-exception v0

    :try_start_1
    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->onCorruption()V

    throw v0
    :try_end_1
    .catchall {:try_start_1 .. :try_end_1} :catchall_0

    :goto_0
    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->releaseReference()V

    throw v0
.end method

.method public simpleQueryForBlobFileDescriptor()Landroid/os/ParcelFileDescriptor;
    .locals 5

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->acquireReference()V

    :try_start_0
    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->getSession()Lorg/sqlite/database/sqlite/SQLiteSession;

    move-result-object v0

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->getSql()Ljava/lang/String;

    move-result-object v1

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->getBindArgs()[Ljava/lang/Object;

    move-result-object v2

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->getConnectionFlags()I

    move-result v3

    const/4 v4, 0x0

    invoke-virtual {v0, v1, v2, v3, v4}, Lorg/sqlite/database/sqlite/SQLiteSession;->executeForBlobFileDescriptor(Ljava/lang/String;[Ljava/lang/Object;ILandroid/os/CancellationSignal;)Landroid/os/ParcelFileDescriptor;

    move-result-object v0
    :try_end_0
    .catch Lorg/sqlite/database/sqlite/SQLiteDatabaseCorruptException; {:try_start_0 .. :try_end_0} :catch_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->releaseReference()V

    return-object v0

    :catchall_0
    move-exception v0

    goto :goto_0

    :catch_0
    move-exception v0

    :try_start_1
    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->onCorruption()V

    throw v0
    :try_end_1
    .catchall {:try_start_1 .. :try_end_1} :catchall_0

    :goto_0
    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->releaseReference()V

    throw v0
.end method

.method public simpleQueryForLong()J
    .locals 5

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->acquireReference()V

    :try_start_0
    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->getSession()Lorg/sqlite/database/sqlite/SQLiteSession;

    move-result-object v0

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->getSql()Ljava/lang/String;

    move-result-object v1

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->getBindArgs()[Ljava/lang/Object;

    move-result-object v2

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->getConnectionFlags()I

    move-result v3

    const/4 v4, 0x0

    invoke-virtual {v0, v1, v2, v3, v4}, Lorg/sqlite/database/sqlite/SQLiteSession;->executeForLong(Ljava/lang/String;[Ljava/lang/Object;ILandroid/os/CancellationSignal;)J

    move-result-wide v0
    :try_end_0
    .catch Lorg/sqlite/database/sqlite/SQLiteDatabaseCorruptException; {:try_start_0 .. :try_end_0} :catch_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->releaseReference()V

    return-wide v0

    :catchall_0
    move-exception v0

    goto :goto_0

    :catch_0
    move-exception v0

    :try_start_1
    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->onCorruption()V

    throw v0
    :try_end_1
    .catchall {:try_start_1 .. :try_end_1} :catchall_0

    :goto_0
    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->releaseReference()V

    throw v0
.end method

.method public simpleQueryForString()Ljava/lang/String;
    .locals 5

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->acquireReference()V

    :try_start_0
    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->getSession()Lorg/sqlite/database/sqlite/SQLiteSession;

    move-result-object v0

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->getSql()Ljava/lang/String;

    move-result-object v1

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->getBindArgs()[Ljava/lang/Object;

    move-result-object v2

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->getConnectionFlags()I

    move-result v3

    const/4 v4, 0x0

    invoke-virtual {v0, v1, v2, v3, v4}, Lorg/sqlite/database/sqlite/SQLiteSession;->executeForString(Ljava/lang/String;[Ljava/lang/Object;ILandroid/os/CancellationSignal;)Ljava/lang/String;

    move-result-object v0
    :try_end_0
    .catch Lorg/sqlite/database/sqlite/SQLiteDatabaseCorruptException; {:try_start_0 .. :try_end_0} :catch_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->releaseReference()V

    return-object v0

    :catchall_0
    move-exception v0

    goto :goto_0

    :catch_0
    move-exception v0

    :try_start_1
    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->onCorruption()V

    throw v0
    :try_end_1
    .catchall {:try_start_1 .. :try_end_1} :catchall_0

    :goto_0
    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->releaseReference()V

    throw v0
.end method

.method public toString()Ljava/lang/String;
    .locals 2

    new-instance v0, Ljava/lang/StringBuilder;

    invoke-direct {v0}, Ljava/lang/StringBuilder;-><init>()V

    sget-object v1, Lorg/sqlite/database/sqlite/SQLiteStatement;->EUTgsl:Ljava/lang/String;

    invoke-virtual {v0, v1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteStatement;->getSql()Ljava/lang/String;

    move-result-object v1

    invoke-virtual {v0, v1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    invoke-virtual {v0}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v0

    return-object v0
.end method
