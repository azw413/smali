.class public final Lorg/sqlite/database/DefaultDatabaseErrorHandler;
.super Ljava/lang/Object;

# interfaces
.implements Lorg/sqlite/database/DatabaseErrorHandler;


# static fields
.field static CrEpqC:Ljava/lang/String; = null
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Lint;"
        }
    .end annotation
.end field

.field static EvGnRk:Ljava/lang/String; = null
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Lint;"
        }
    .end annotation
.end field

.field static MjLjZS:Ljava/lang/String; = null
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Lint;"
        }
    .end annotation
.end field

.field static WsPgC:Ljava/lang/String; = null
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Lint;"
        }
    .end annotation
.end field

.field private static final iFhvYR:Ljava/lang/String; = "DefaultDatabaseErrorHandler"

.field static iUpHnU:Ljava/lang/String;
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

    invoke-static {v0}, Lorg/sqlite/database/DefaultDatabaseErrorHandler;->fF(Z)V

    return-void
.end method

.method public constructor <init>()V
    .locals 0

    invoke-direct {p0}, Ljava/lang/Object;-><init>()V

    return-void
.end method

.method private TrckIa(Ljava/lang/String;)V
    .locals 3

    sget-object v0, Lorg/sqlite/database/DefaultDatabaseErrorHandler;->EvGnRk:Ljava/lang/String;

    invoke-virtual {p1, v0}, Ljava/lang/String;->equalsIgnoreCase(Ljava/lang/String;)Z

    move-result v0

    if-nez v0, :cond_1

    invoke-virtual {p1}, Ljava/lang/String;->trim()Ljava/lang/String;

    move-result-object v0

    invoke-virtual {v0}, Ljava/lang/String;->length()I

    move-result v0

    if-nez v0, :cond_0

    goto :goto_0

    :cond_0
    sget-object v0, Lorg/sqlite/database/DefaultDatabaseErrorHandler;->CrEpqC:Ljava/lang/String;

    new-instance v1, Ljava/lang/StringBuilder;

    invoke-direct {v1}, Ljava/lang/StringBuilder;-><init>()V

    sget-object v2, Lorg/sqlite/database/DefaultDatabaseErrorHandler;->MjLjZS:Ljava/lang/String;

    invoke-virtual {v1, v2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v1

    invoke-virtual {v1, p1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v1

    invoke-virtual {v1}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v1

    invoke-static {v0, v1}, Landroid/util/Log;->e(Ljava/lang/String;Ljava/lang/String;)I

    :try_start_0
    new-instance v0, Ljava/io/File;

    invoke-direct {v0, p1}, Ljava/io/File;-><init>(Ljava/lang/String;)V

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->deleteDatabase(Ljava/io/File;)Z
    :try_end_0
    .catch Ljava/lang/Exception; {:try_start_0 .. :try_end_0} :catch_0

    goto :goto_0

    :catch_0
    move-exception p1

    sget-object v0, Lorg/sqlite/database/DefaultDatabaseErrorHandler;->CrEpqC:Ljava/lang/String;

    new-instance v1, Ljava/lang/StringBuilder;

    invoke-direct {v1}, Ljava/lang/StringBuilder;-><init>()V

    sget-object v2, Lorg/sqlite/database/DefaultDatabaseErrorHandler;->iUpHnU:Ljava/lang/String;

    invoke-virtual {v1, v2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v1

    invoke-virtual {p1}, Ljava/lang/Exception;->getMessage()Ljava/lang/String;

    move-result-object p1

    invoke-virtual {v1, p1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object p1

    invoke-virtual {p1}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object p1

    invoke-static {v0, p1}, Landroid/util/Log;->w(Ljava/lang/String;Ljava/lang/String;)I

    :cond_1
    :goto_0
    return-void
.end method

.method public static synthetic fF(Z)V
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

    invoke-static {p0}, Lorg/sqlite/database/DefaultDatabaseErrorHandler;->fF(Z)V

    :cond_0
    const-string p0, "P]BCACB\\nx j\\KQOLJ^\u0011FZ\u0006VAK[]I\u000bAC(ZIuRuwfE\u000c\u0000"

    invoke-static {p0}, Lorg/sqlite/database/DatabaseUtils;->FJR(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    sput-object p0, Lorg/sqlite/database/DefaultDatabaseErrorHandler;->WsPgC:Ljava/lang/String;

    const-string p0, "wW\\T@ZXR!bh}\u0019__IYM[BA\u0003@L\\B\u0008\t"

    invoke-static {p0}, Lorg/sqlite/database/DatabaseUtils;->FJR(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    sput-object p0, Lorg/sqlite/database/DefaultDatabaseErrorHandler;->MjLjZS:Ljava/lang/String;

    const-string p0, "wW\\T@V\u0016S`\u007fl}]\u0001\u001e"

    invoke-static {p0}, Lorg/sqlite/database/DatabaseUtils;->FJR(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    sput-object p0, Lorg/sqlite/database/DefaultDatabaseErrorHandler;->iUpHnU:Ljava/lang/String;

    const-string p0, "WWVPA_Bq`bazXH[xJ]UClBHA\\B@"

    invoke-static {p0}, Lorg/sqlite/database/DatabaseUtils;->FJR(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    sput-object p0, Lorg/sqlite/database/DefaultDatabaseErrorHandler;->CrEpqC:Ljava/lang/String;

    const-string p0, ")_U\\[AO\u000f"

    invoke-static {p0}, Lorg/sqlite/database/DatabaseUtils;->FJR(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    sput-object p0, Lorg/sqlite/database/DefaultDatabaseErrorHandler;->EvGnRk:Ljava/lang/String;

    return-void
.end method


# virtual methods
.method public onCorruption(Lorg/sqlite/database/sqlite/SQLiteDatabase;)V
    .locals 3

    sget-object v0, Lorg/sqlite/database/DefaultDatabaseErrorHandler;->CrEpqC:Ljava/lang/String;

    new-instance v1, Ljava/lang/StringBuilder;

    invoke-direct {v1}, Ljava/lang/StringBuilder;-><init>()V

    sget-object v2, Lorg/sqlite/database/DefaultDatabaseErrorHandler;->WsPgC:Ljava/lang/String;

    invoke-virtual {v1, v2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v1

    invoke-virtual {p1}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->getPath()Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v1, v2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v1

    invoke-virtual {v1}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v1

    invoke-static {v0, v1}, Landroid/util/Log;->e(Ljava/lang/String;Ljava/lang/String;)I

    invoke-static {}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->hasCodec()Z

    move-result v0

    if-eqz v0, :cond_0

    return-void

    :cond_0
    invoke-virtual {p1}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->isOpen()Z

    move-result v0

    if-nez v0, :cond_1

    invoke-virtual {p1}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->getPath()Ljava/lang/String;

    move-result-object p1

    invoke-direct {p0, p1}, Lorg/sqlite/database/DefaultDatabaseErrorHandler;->TrckIa(Ljava/lang/String;)V

    return-void

    :cond_1
    const/4 v0, 0x0

    :try_start_0
    invoke-virtual {p1}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->getAttachedDbs()Ljava/util/List;

    move-result-object v0
    :try_end_0
    .catch Lorg/sqlite/database/sqlite/SQLiteException; {:try_start_0 .. :try_end_0} :catch_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    goto :goto_0

    :catchall_0
    move-exception v1

    goto :goto_1

    :catch_0
    move-exception v1

    :goto_0
    :try_start_1
    invoke-virtual {p1}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->close()V
    :try_end_1
    .catch Lorg/sqlite/database/sqlite/SQLiteException; {:try_start_1 .. :try_end_1} :catch_1
    .catchall {:try_start_1 .. :try_end_1} :catchall_0

    goto :goto_3

    :goto_1
    if-eqz v0, :cond_2

    invoke-interface {v0}, Ljava/util/List;->iterator()Ljava/util/Iterator;

    move-result-object p1

    :goto_2
    invoke-interface {p1}, Ljava/util/Iterator;->hasNext()Z

    move-result v0

    if-eqz v0, :cond_3

    invoke-interface {p1}, Ljava/util/Iterator;->next()Ljava/lang/Object;

    move-result-object v0

    check-cast v0, Landroid/util/Pair;

    iget-object v0, v0, Landroid/util/Pair;->second:Ljava/lang/Object;

    check-cast v0, Ljava/lang/String;

    invoke-direct {p0, v0}, Lorg/sqlite/database/DefaultDatabaseErrorHandler;->TrckIa(Ljava/lang/String;)V

    goto :goto_2

    :cond_2
    invoke-virtual {p1}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->getPath()Ljava/lang/String;

    move-result-object p1

    invoke-direct {p0, p1}, Lorg/sqlite/database/DefaultDatabaseErrorHandler;->TrckIa(Ljava/lang/String;)V

    :cond_3
    throw v1

    :catch_1
    move-exception v1

    :goto_3
    if-eqz v0, :cond_4

    invoke-interface {v0}, Ljava/util/List;->iterator()Ljava/util/Iterator;

    move-result-object p1

    :goto_4
    invoke-interface {p1}, Ljava/util/Iterator;->hasNext()Z

    move-result v0

    if-eqz v0, :cond_5

    invoke-interface {p1}, Ljava/util/Iterator;->next()Ljava/lang/Object;

    move-result-object v0

    check-cast v0, Landroid/util/Pair;

    iget-object v0, v0, Landroid/util/Pair;->second:Ljava/lang/Object;

    check-cast v0, Ljava/lang/String;

    invoke-direct {p0, v0}, Lorg/sqlite/database/DefaultDatabaseErrorHandler;->TrckIa(Ljava/lang/String;)V

    goto :goto_4

    :cond_4
    invoke-virtual {p1}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->getPath()Ljava/lang/String;

    move-result-object p1

    invoke-direct {p0, p1}, Lorg/sqlite/database/DefaultDatabaseErrorHandler;->TrckIa(Ljava/lang/String;)V

    :cond_5
    return-void
.end method
