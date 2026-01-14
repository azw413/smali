.class final Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatementCache;
.super Landroid/util/LruCache;


# annotations
.annotation system Ldalvik/annotation/EnclosingClass;
    value = Lorg/sqlite/database/sqlite/SQLiteConnection;
.end annotation

.annotation system Ldalvik/annotation/InnerClass;
    accessFlags = 0x12
    name = "PreparedStatementCache"
.end annotation

.annotation system Ldalvik/annotation/Signature;
    value = {
        "Landroid/util/LruCache<",
        "Ljava/lang/String;",
        "Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;",
        ">;"
    }
.end annotation


# static fields
.field static CISZHS:Ljava/lang/String;
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Lint;"
        }
    .end annotation
.end field

.field static EtChrI:Ljava/lang/String;
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Lint;"
        }
    .end annotation
.end field

.field static MgdGCp:Ljava/lang/String;
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Lint;"
        }
    .end annotation
.end field

.field static QnbraG:Ljava/lang/String;
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Lint;"
        }
    .end annotation
.end field

.field static YhNWCS:Ljava/lang/String;
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Lint;"
        }
    .end annotation
.end field

.field static eZWCRE:Ljava/lang/String;
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Lint;"
        }
    .end annotation
.end field

.field static ezIMTD:Ljava/lang/String;
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Lint;"
        }
    .end annotation
.end field

.field static ikSEjY:Ljava/lang/String;
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Lint;"
        }
    .end annotation
.end field


# instance fields
.field final synthetic QxtthM:Lorg/sqlite/database/sqlite/SQLiteConnection;


# direct methods
.method static constructor <clinit>()V
    .locals 1

    const/4 v0, 0x0

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatementCache;->PZD(Z)V

    return-void
.end method

.method public constructor <init>(Lorg/sqlite/database/sqlite/SQLiteConnection;I)V
    .locals 0

    iput-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatementCache;->QxtthM:Lorg/sqlite/database/sqlite/SQLiteConnection;

    invoke-direct {p0, p2}, Landroid/util/LruCache;-><init>(I)V

    return-void
.end method

.method public static synthetic PZD(Z)V
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

    invoke-static {p0}, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatementCache;->PZD(Z)V

    :cond_0
    const-string p0, "7+]P\\N\u0013"

    invoke-static {p0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    sput-object p0, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatementCache;->ezIMTD:Ljava/lang/String;

    const-string p0, "!+Z]M_K@Mq~pVU\u001b\u0015H"

    invoke-static {p0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    sput-object p0, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatementCache;->QnbraG:Ljava/lang/String;

    const-string p0, ";+\t\t"

    invoke-static {p0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    sput-object p0, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatementCache;->YhNWCS:Ljava/lang/String;

    const-string p0, ";+\t\t\u0010EACM!"

    invoke-static {p0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    sput-object p0, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatementCache;->MgdGCp:Ljava/lang/String;

    const-string p0, "7+[LMOaCDf7"

    invoke-static {p0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    sput-object p0, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatementCache;->CISZHS:Ljava/lang/String;

    const-string p0, ";+y[I[O_M{*SVFR@]B\\]\u001cX_^PJ\u0000"

    invoke-static {p0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    sput-object p0, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatementCache;->eZWCRE:Ljava/lang/String;

    const-string p0, "7+ZX@\u0016\u000c"

    invoke-static {p0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    sput-object p0, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatementCache;->ikSEjY:Ljava/lang/String;

    const-string p0, "7+G\\A{O_IroTGUU\u0018"

    invoke-static {p0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    sput-object p0, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatementCache;->EtChrI:Ljava/lang/String;

    return-void
.end method


# virtual methods
.method public dump(Landroid/util/Printer;)V
    .locals 7

    sget-object v0, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatementCache;->eZWCRE:Ljava/lang/String;

    invoke-interface {p1, v0}, Landroid/util/Printer;->println(Ljava/lang/String;)V

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatementCache;->snapshot()Ljava/util/Map;

    move-result-object v0

    invoke-interface {v0}, Ljava/util/Map;->isEmpty()Z

    move-result v1

    if-nez v1, :cond_1

    invoke-interface {v0}, Ljava/util/Map;->entrySet()Ljava/util/Set;

    move-result-object v0

    invoke-interface {v0}, Ljava/util/Set;->iterator()Ljava/util/Iterator;

    move-result-object v0

    const/4 v1, 0x0

    :goto_0
    invoke-interface {v0}, Ljava/util/Iterator;->hasNext()Z

    move-result v2

    if-eqz v2, :cond_2

    invoke-interface {v0}, Ljava/util/Iterator;->next()Ljava/lang/Object;

    move-result-object v2

    check-cast v2, Ljava/util/Map$Entry;

    invoke-interface {v2}, Ljava/util/Map$Entry;->getValue()Ljava/lang/Object;

    move-result-object v3

    check-cast v3, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;

    iget-boolean v4, v3, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;->mInCache:Z

    if-eqz v4, :cond_0

    invoke-interface {v2}, Ljava/util/Map$Entry;->getKey()Ljava/lang/Object;

    move-result-object v2

    check-cast v2, Ljava/lang/String;

    new-instance v4, Ljava/lang/StringBuilder;

    invoke-direct {v4}, Ljava/lang/StringBuilder;-><init>()V

    sget-object v5, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatementCache;->YhNWCS:Ljava/lang/String;

    invoke-virtual {v4, v5}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v4

    invoke-virtual {v4, v1}, Ljava/lang/StringBuilder;->append(I)Ljava/lang/StringBuilder;

    move-result-object v4

    sget-object v5, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatementCache;->QnbraG:Ljava/lang/String;

    invoke-virtual {v4, v5}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v4

    iget-wide v5, v3, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;->mStatementPtr:J

    invoke-static {v5, v6}, Ljava/lang/Long;->toHexString(J)Ljava/lang/String;

    move-result-object v5

    invoke-virtual {v4, v5}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v4

    sget-object v5, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatementCache;->EtChrI:Ljava/lang/String;

    invoke-virtual {v4, v5}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v4

    iget v5, v3, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;->mNumParameters:I

    invoke-virtual {v4, v5}, Ljava/lang/StringBuilder;->append(I)Ljava/lang/StringBuilder;

    move-result-object v4

    sget-object v5, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatementCache;->ezIMTD:Ljava/lang/String;

    invoke-virtual {v4, v5}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v4

    iget v5, v3, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;->mType:I

    invoke-virtual {v4, v5}, Ljava/lang/StringBuilder;->append(I)Ljava/lang/StringBuilder;

    move-result-object v4

    sget-object v5, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatementCache;->CISZHS:Ljava/lang/String;

    invoke-virtual {v4, v5}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v4

    iget-boolean v3, v3, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;->mReadOnly:Z

    invoke-virtual {v4, v3}, Ljava/lang/StringBuilder;->append(Z)Ljava/lang/StringBuilder;

    move-result-object v3

    sget-object v4, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatementCache;->ikSEjY:Ljava/lang/String;

    invoke-virtual {v3, v4}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v3

    invoke-static {v2}, Lorg/sqlite/database/sqlite/SQLiteConnection;->access$300(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v3, v2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v2

    const-string v3, "\""

    invoke-virtual {v2, v3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v2

    invoke-virtual {v2}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v2

    invoke-interface {p1, v2}, Landroid/util/Printer;->println(Ljava/lang/String;)V

    :cond_0
    add-int/lit8 v1, v1, 0x1

    goto :goto_0

    :cond_1
    sget-object v0, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatementCache;->MgdGCp:Ljava/lang/String;

    invoke-interface {p1, v0}, Landroid/util/Printer;->println(Ljava/lang/String;)V

    :cond_2
    return-void
.end method

.method protected bridge synthetic entryRemoved(ZLjava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)V
    .locals 0

    check-cast p2, Ljava/lang/String;

    check-cast p3, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;

    check-cast p4, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;

    invoke-virtual {p0, p1, p2, p3, p4}, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatementCache;->entryRemoved(ZLjava/lang/String;Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;)V

    return-void
.end method

.method protected entryRemoved(ZLjava/lang/String;Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;)V
    .locals 0

    const/4 p1, 0x0

    iput-boolean p1, p3, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;->mInCache:Z

    iget-boolean p1, p3, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;->mInUse:Z

    if-nez p1, :cond_0

    iget-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatementCache;->QxtthM:Lorg/sqlite/database/sqlite/SQLiteConnection;

    invoke-static {p1, p3}, Lorg/sqlite/database/sqlite/SQLiteConnection;->access$200(Lorg/sqlite/database/sqlite/SQLiteConnection;Lorg/sqlite/database/sqlite/SQLiteConnection$PreparedStatement;)V

    :cond_0
    return-void
.end method
