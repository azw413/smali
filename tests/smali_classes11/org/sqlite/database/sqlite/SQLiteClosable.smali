.class public abstract Lorg/sqlite/database/sqlite/SQLiteClosable;
.super Ljava/lang/Object;

# interfaces
.implements Ljava/io/Closeable;


# static fields
.field static ENNEGP:Ljava/lang/String;
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Lint;"
        }
    .end annotation
.end field


# instance fields
.field private YqaYaE:I


# direct methods
.method static constructor <clinit>()V
    .locals 1

    const/4 v0, 0x0

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteClosable;->bQ(Z)V

    return-void
.end method

.method public constructor <init>()V
    .locals 1

    invoke-direct {p0}, Ljava/lang/Object;-><init>()V

    const/4 v0, 0x1

    iput v0, p0, Lorg/sqlite/database/sqlite/SQLiteClosable;->YqaYaE:I

    return-void
.end method

.method public static synthetic bQ(Z)V
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

    invoke-static {p0}, Lorg/sqlite/database/sqlite/SQLiteClosable;->bQ(Z)V

    :cond_0
    const-string p0, "z\u007f]LA[Z\r\\p*RG\nIUUI\u0012HR\u001b_QJJ[UM\u001eUYOEE\\\n`lgm]\\;$"

    invoke-static {p0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    sput-object p0, Lorg/sqlite/database/sqlite/SQLiteClosable;->ENNEGP:Ljava/lang/String;

    return-void
.end method


# virtual methods
.method public acquireReference()V
    .locals 3

    monitor-enter p0

    :try_start_0
    iget v0, p0, Lorg/sqlite/database/sqlite/SQLiteClosable;->YqaYaE:I

    if-lez v0, :cond_0

    add-int/lit8 v0, v0, 0x1

    iput v0, p0, Lorg/sqlite/database/sqlite/SQLiteClosable;->YqaYaE:I

    monitor-exit p0

    return-void

    :cond_0
    new-instance v0, Ljava/lang/IllegalStateException;

    new-instance v1, Ljava/lang/StringBuilder;

    invoke-direct {v1}, Ljava/lang/StringBuilder;-><init>()V

    sget-object v2, Lorg/sqlite/database/sqlite/SQLiteClosable;->ENNEGP:Ljava/lang/String;

    invoke-virtual {v1, v2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v1

    invoke-virtual {v1, p0}, Ljava/lang/StringBuilder;->append(Ljava/lang/Object;)Ljava/lang/StringBuilder;

    move-result-object v1

    invoke-virtual {v1}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v1

    invoke-direct {v0, v1}, Ljava/lang/IllegalStateException;-><init>(Ljava/lang/String;)V

    throw v0

    :catchall_0
    move-exception v0

    monitor-exit p0
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    throw v0
.end method

.method public close()V
    .locals 0

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteClosable;->releaseReference()V

    return-void
.end method

.method protected abstract onAllReferencesReleased()V
.end method

.method protected onAllReferencesReleasedFromContainer()V
    .locals 0
    .annotation runtime Ljava/lang/Deprecated;
    .end annotation

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteClosable;->onAllReferencesReleased()V

    return-void
.end method

.method public releaseReference()V
    .locals 2

    monitor-enter p0

    :try_start_0
    iget v0, p0, Lorg/sqlite/database/sqlite/SQLiteClosable;->YqaYaE:I

    const/4 v1, 0x1

    sub-int/2addr v0, v1

    iput v0, p0, Lorg/sqlite/database/sqlite/SQLiteClosable;->YqaYaE:I

    if-nez v0, :cond_0

    goto :goto_0

    :cond_0
    const/4 v1, 0x0

    :goto_0
    monitor-exit p0
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    if-eqz v1, :cond_1

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteClosable;->onAllReferencesReleased()V

    :cond_1
    return-void

    :catchall_0
    move-exception v0

    :try_start_1
    monitor-exit p0
    :try_end_1
    .catchall {:try_start_1 .. :try_end_1} :catchall_0

    throw v0
.end method

.method public releaseReferenceFromContainer()V
    .locals 2
    .annotation runtime Ljava/lang/Deprecated;
    .end annotation

    monitor-enter p0

    :try_start_0
    iget v0, p0, Lorg/sqlite/database/sqlite/SQLiteClosable;->YqaYaE:I

    const/4 v1, 0x1

    sub-int/2addr v0, v1

    iput v0, p0, Lorg/sqlite/database/sqlite/SQLiteClosable;->YqaYaE:I

    if-nez v0, :cond_0

    goto :goto_0

    :cond_0
    const/4 v1, 0x0

    :goto_0
    monitor-exit p0
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    if-eqz v1, :cond_1

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteClosable;->onAllReferencesReleasedFromContainer()V

    :cond_1
    return-void

    :catchall_0
    move-exception v0

    :try_start_1
    monitor-exit p0
    :try_end_1
    .catchall {:try_start_1 .. :try_end_1} :catchall_0

    throw v0
.end method
