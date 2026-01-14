.class public abstract Lorg/sqlite/database/sqlite/SQLiteOpenHelper;
.super Ljava/lang/Object;


# static fields
.field private static final eOLSOI:Z = false

.field private static final iFhvYR:Ljava/lang/String;


# instance fields
.field private final BMDCDi:Lorg/sqlite/database/sqlite/SQLiteDatabase$CursorFactory;

.field private CmrnFC:Z

.field private final EsEqXk:I

.field private final TqOptV:Ljava/lang/String;

.field private YjPJqf:Lorg/sqlite/database/sqlite/SQLiteDatabase;

.field private final YmCDDL:Lorg/sqlite/database/DatabaseErrorHandler;

.field private final ednkXz:I

.field private final iquHnI:Landroid/content/Context;

.field private mZNmRi:Z


# direct methods
.method static constructor <clinit>()V
    .locals 1

    const-class v0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;

    invoke-virtual {v0}, Ljava/lang/Class;->getSimpleName()Ljava/lang/String;

    move-result-object v0

    sput-object v0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->iFhvYR:Ljava/lang/String;

    return-void
.end method

.method public constructor <init>(Landroid/content/Context;Ljava/lang/String;Lorg/sqlite/database/sqlite/SQLiteDatabase$CursorFactory;I)V
    .locals 6

    const/4 v5, 0x0

    move-object v0, p0

    move-object v1, p1

    move-object v2, p2

    move-object v3, p3

    move v4, p4

    invoke-direct/range {v0 .. v5}, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;-><init>(Landroid/content/Context;Ljava/lang/String;Lorg/sqlite/database/sqlite/SQLiteDatabase$CursorFactory;ILorg/sqlite/database/DatabaseErrorHandler;)V

    return-void
.end method

.method public constructor <init>(Landroid/content/Context;Ljava/lang/String;Lorg/sqlite/database/sqlite/SQLiteDatabase$CursorFactory;IILorg/sqlite/database/DatabaseErrorHandler;)V
    .locals 1

    invoke-direct {p0}, Ljava/lang/Object;-><init>()V

    const/4 v0, 0x1

    if-lt p4, v0, :cond_0

    iput-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->iquHnI:Landroid/content/Context;

    iput-object p2, p0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->TqOptV:Ljava/lang/String;

    iput-object p3, p0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->BMDCDi:Lorg/sqlite/database/sqlite/SQLiteDatabase$CursorFactory;

    iput p4, p0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->EsEqXk:I

    iput-object p6, p0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->YmCDDL:Lorg/sqlite/database/DatabaseErrorHandler;

    const/4 p1, 0x0

    invoke-static {p1, p5}, Ljava/lang/Math;->max(II)I

    move-result p1

    iput p1, p0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->ednkXz:I

    return-void

    :cond_0
    new-instance p1, Ljava/lang/IllegalArgumentException;

    new-instance p2, Ljava/lang/StringBuilder;

    invoke-direct {p2}, Ljava/lang/StringBuilder;-><init>()V

    const-string p3, "Mn[ZED@\rEjyT\u0002EC\u0005\u000e\u001a\u0012\u0018\u0010\u001bI\\K\u000f"

    invoke-static {p3}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p3

    invoke-virtual {p2, p3}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object p2

    invoke-virtual {p2, p4}, Ljava/lang/StringBuilder;->append(I)Ljava/lang/StringBuilder;

    move-result-object p2

    invoke-virtual {p2}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object p2

    invoke-direct {p1, p2}, Ljava/lang/IllegalArgumentException;-><init>(Ljava/lang/String;)V

    throw p1
.end method

.method public constructor <init>(Landroid/content/Context;Ljava/lang/String;Lorg/sqlite/database/sqlite/SQLiteDatabase$CursorFactory;ILorg/sqlite/database/DatabaseErrorHandler;)V
    .locals 7

    const/4 v5, 0x0

    move-object v0, p0

    move-object v1, p1

    move-object v2, p2

    move-object v3, p3

    move v4, p4

    move-object v6, p5

    invoke-direct/range {v0 .. v6}, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;-><init>(Landroid/content/Context;Ljava/lang/String;Lorg/sqlite/database/sqlite/SQLiteDatabase$CursorFactory;IILorg/sqlite/database/DatabaseErrorHandler;)V

    return-void
.end method

.method private QOuBxx(Z)Lorg/sqlite/database/sqlite/SQLiteDatabase;
    .locals 8

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->YjPJqf:Lorg/sqlite/database/sqlite/SQLiteDatabase;

    const/4 v1, 0x0

    if-eqz v0, :cond_2

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->isOpen()Z

    move-result v0

    if-nez v0, :cond_0

    iput-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->YjPJqf:Lorg/sqlite/database/sqlite/SQLiteDatabase;

    goto :goto_0

    :cond_0
    if-eqz p1, :cond_1

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->YjPJqf:Lorg/sqlite/database/sqlite/SQLiteDatabase;

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->isReadOnly()Z

    move-result v0

    if-nez v0, :cond_2

    :cond_1
    iget-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->YjPJqf:Lorg/sqlite/database/sqlite/SQLiteDatabase;

    return-object p1

    :cond_2
    :goto_0
    iget-boolean v0, p0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->mZNmRi:Z

    if-nez v0, :cond_11

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->YjPJqf:Lorg/sqlite/database/sqlite/SQLiteDatabase;

    const/4 v2, 0x1

    const/4 v3, 0x0

    :try_start_0
    iput-boolean v2, p0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->mZNmRi:Z

    if-eqz v0, :cond_3

    if-eqz p1, :cond_6

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->isReadOnly()Z

    move-result v1

    if-eqz v1, :cond_6

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->reopenReadWrite()V

    goto :goto_1

    :cond_3
    iget-object v4, p0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->TqOptV:Ljava/lang/String;

    if-nez v4, :cond_4

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->create(Lorg/sqlite/database/sqlite/SQLiteDatabase$CursorFactory;)Lorg/sqlite/database/sqlite/SQLiteDatabase;

    move-result-object v0

    goto :goto_1

    :cond_4
    const-string v1, "}bEL\u0016"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    invoke-virtual {v4, v1}, Ljava/lang/String;->startsWith(Ljava/lang/String;)Z

    move-result v1

    if-nez v1, :cond_5

    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->iquHnI:Landroid/content/Context;

    invoke-virtual {v1, v4}, Landroid/content/Context;->getDatabasePath(Ljava/lang/String;)Ljava/io/File;

    move-result-object v1

    invoke-virtual {v1}, Ljava/io/File;->getPath()Ljava/lang/String;

    move-result-object v4
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_1

    :cond_5
    :try_start_1
    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->BMDCDi:Lorg/sqlite/database/sqlite/SQLiteDatabase$CursorFactory;

    iget-object v5, p0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->YmCDDL:Lorg/sqlite/database/DatabaseErrorHandler;

    invoke-static {v4, v1, v5}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->openOrCreateDatabase(Ljava/lang/String;Lorg/sqlite/database/sqlite/SQLiteDatabase$CursorFactory;Lorg/sqlite/database/DatabaseErrorHandler;)Lorg/sqlite/database/sqlite/SQLiteDatabase;

    move-result-object v0
    :try_end_1
    .catch Lorg/sqlite/database/sqlite/SQLiteException; {:try_start_1 .. :try_end_1} :catch_0
    .catchall {:try_start_1 .. :try_end_1} :catchall_1

    goto :goto_1

    :catch_0
    move-exception v1

    if-nez p1, :cond_f

    :try_start_2
    sget-object v5, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->iFhvYR:Ljava/lang/String;

    new-instance v6, Ljava/lang/StringBuilder;

    invoke-direct {v6}, Ljava/lang/StringBuilder;-><init>()V

    const-string v7, "Xd\\EHE\tY\u0008pzEL\u0007"

    invoke-static {v7}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v7

    invoke-virtual {v6, v7}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v6

    iget-object v7, p0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->TqOptV:Ljava/lang/String;

    invoke-virtual {v6, v7}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v6

    const-string v7, ";mF[\u000c\\\\D\\vdG\u0002\u000fQL\\K\u0012]NB\u001eO]N^\u001c[]ZL\t\u000c"

    invoke-static {v7}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v7

    invoke-virtual {v6, v7}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v6

    invoke-virtual {v6}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v6

    invoke-static {v5, v6, v1}, Landroid/util/Log;->e(Ljava/lang/String;Ljava/lang/String;Ljava/lang/Throwable;)I

    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->BMDCDi:Lorg/sqlite/database/sqlite/SQLiteDatabase$CursorFactory;

    iget-object v5, p0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->YmCDDL:Lorg/sqlite/database/DatabaseErrorHandler;

    invoke-static {v4, v1, v2, v5}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->openDatabase(Ljava/lang/String;Lorg/sqlite/database/sqlite/SQLiteDatabase$CursorFactory;ILorg/sqlite/database/DatabaseErrorHandler;)Lorg/sqlite/database/sqlite/SQLiteDatabase;

    move-result-object v0

    :cond_6
    :goto_1
    invoke-virtual {p0, v0}, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->onConfigure(Lorg/sqlite/database/sqlite/SQLiteDatabase;)V

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->getVersion()I

    move-result v1

    iget v2, p0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->EsEqXk:I

    if-eq v1, v2, :cond_d

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->isReadOnly()Z

    move-result v2

    if-nez v2, :cond_c

    if-lez v1, :cond_9

    iget v2, p0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->ednkXz:I

    if-ge v1, v2, :cond_9

    new-instance v2, Ljava/io/File;

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->getPath()Ljava/lang/String;

    move-result-object v4

    invoke-direct {v2, v4}, Ljava/io/File;-><init>(Ljava/lang/String;)V

    invoke-virtual {p0, v0}, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->onBeforeDelete(Lorg/sqlite/database/sqlite/SQLiteDatabase;)V

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->close()V

    invoke-static {v2}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->deleteDatabase(Ljava/io/File;)Z

    move-result v2

    if-eqz v2, :cond_8

    iput-boolean v3, p0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->mZNmRi:Z

    invoke-direct {p0, p1}, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->QOuBxx(Z)Lorg/sqlite/database/sqlite/SQLiteDatabase;

    move-result-object p1
    :try_end_2
    .catchall {:try_start_2 .. :try_end_2} :catchall_1

    iput-boolean v3, p0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->mZNmRi:Z

    if-eqz v0, :cond_7

    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->YjPJqf:Lorg/sqlite/database/sqlite/SQLiteDatabase;

    if-eq v0, v1, :cond_7

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->close()V

    :cond_7
    return-object p1

    :cond_8
    :try_start_3
    new-instance p1, Ljava/lang/IllegalStateException;

    new-instance v2, Ljava/lang/StringBuilder;

    invoke-direct {v2}, Ljava/lang/StringBuilder;-><init>()V

    const-string v4, "NeHK@N\u000eYG?nENBR@\u0010HPZSW[I]\u000f^P@RTTSS\u0000"

    invoke-static {v4}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v4

    invoke-virtual {v2, v4}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v2

    iget-object v4, p0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->TqOptV:Ljava/lang/String;

    invoke-virtual {v2, v4}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v2

    const-string v4, ";|@]D\u000bXHZlcOL\u0007"

    invoke-static {v4}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v4

    invoke-virtual {v2, v4}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v2

    invoke-virtual {v2, v1}, Ljava/lang/StringBuilder;->append(I)Ljava/lang/StringBuilder;

    move-result-object v1

    invoke-virtual {v1}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v1

    invoke-direct {p1, v1}, Ljava/lang/IllegalStateException;-><init>(Ljava/lang/String;)V

    throw p1

    :cond_9
    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->YjTKMU()V
    :try_end_3
    .catchall {:try_start_3 .. :try_end_3} :catchall_1

    if-nez v1, :cond_a

    :try_start_4
    invoke-virtual {p0, v0}, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->onCreate(Lorg/sqlite/database/sqlite/SQLiteDatabase;)V

    goto :goto_2

    :cond_a
    iget p1, p0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->EsEqXk:I

    if-le v1, p1, :cond_b

    invoke-virtual {p0, v0, v1, p1}, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->onDowngrade(Lorg/sqlite/database/sqlite/SQLiteDatabase;II)V

    goto :goto_2

    :cond_b
    invoke-virtual {p0, v0, v1, p1}, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->onUpgrade(Lorg/sqlite/database/sqlite/SQLiteDatabase;II)V

    :goto_2
    iget p1, p0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->EsEqXk:I

    invoke-virtual {v0, p1}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->setVersion(I)V

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->setTransactionSuccessful()V
    :try_end_4
    .catchall {:try_start_4 .. :try_end_4} :catchall_0

    :try_start_5
    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->endTransaction()V

    goto :goto_3

    :catchall_0
    move-exception p1

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->endTransaction()V

    throw p1

    :cond_c
    new-instance p1, Lorg/sqlite/database/sqlite/SQLiteException;

    new-instance v1, Ljava/lang/StringBuilder;

    invoke-direct {v1}, Ljava/lang/StringBuilder;-><init>()V

    const-string v2, "XjG\u000eX\u000b[]OmkDG\u0007T@QC\u001fFRWG\u001d\\NNPVREP\u0000PRWG/xhzMAnj#"

    invoke-static {v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v1, v2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v1

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->getVersion()I

    move-result v2

    invoke-virtual {v1, v2}, Ljava/lang/StringBuilder;->append(I)Ljava/lang/StringBuilder;

    move-result-object v1

    const-string v2, ";\u007fF\t"

    invoke-static {v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v1, v2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v1

    iget v2, p0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->EsEqXk:I

    invoke-virtual {v1, v2}, Ljava/lang/StringBuilder;->append(I)Ljava/lang/StringBuilder;

    move-result-object v1

    const-string v2, "!+"

    invoke-static {v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v1, v2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v1

    iget-object v2, p0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->TqOptV:Ljava/lang/String;

    invoke-virtual {v1, v2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v1

    invoke-virtual {v1}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v1

    invoke-direct {p1, v1}, Lorg/sqlite/database/sqlite/SQLiteException;-><init>(Ljava/lang/String;)V

    throw p1

    :cond_d
    :goto_3
    invoke-virtual {p0, v0}, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->onOpen(Lorg/sqlite/database/sqlite/SQLiteDatabase;)V

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->isReadOnly()Z

    move-result p1

    if-eqz p1, :cond_e

    sget-object p1, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->iFhvYR:Ljava/lang/String;

    new-instance v1, Ljava/lang/StringBuilder;

    invoke-direct {v1}, Ljava/lang/StringBuilder;-><init>()V

    const-string v2, "T{LGIO\u000e"

    invoke-static {v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v1, v2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v1

    iget-object v2, p0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->TqOptV:Ljava/lang/String;

    invoke-virtual {v1, v2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v1

    const-string v2, ";bG\t^NOI\u0005pdL[\u0007KJTB"

    invoke-static {v2}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v2

    invoke-virtual {v1, v2}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v1

    invoke-virtual {v1}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v1

    invoke-static {p1, v1}, Landroid/util/Log;->w(Ljava/lang/String;Ljava/lang/String;)I

    :cond_e
    iput-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->YjPJqf:Lorg/sqlite/database/sqlite/SQLiteDatabase;
    :try_end_5
    .catchall {:try_start_5 .. :try_end_5} :catchall_1

    iput-boolean v3, p0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->mZNmRi:Z

    return-object v0

    :cond_f
    :try_start_6
    throw v1
    :try_end_6
    .catchall {:try_start_6 .. :try_end_6} :catchall_1

    :catchall_1
    move-exception p1

    iput-boolean v3, p0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->mZNmRi:Z

    if-eqz v0, :cond_10

    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->YjPJqf:Lorg/sqlite/database/sqlite/SQLiteDatabase;

    if-eq v0, v1, :cond_10

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->close()V

    :cond_10
    throw p1

    :cond_11
    new-instance p1, Ljava/lang/IllegalStateException;

    const-string v0, "|n]mM_OOIlo\u0000AFJIUC\u0012[YXKOKFLTXJ"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v0

    invoke-direct {p1, v0}, Ljava/lang/IllegalStateException;-><init>(Ljava/lang/String;)V

    throw p1
.end method


# virtual methods
.method public declared-synchronized close()V
    .locals 2

    monitor-enter p0

    :try_start_0
    iget-boolean v0, p0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->mZNmRi:Z

    if-nez v0, :cond_1

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->YjPJqf:Lorg/sqlite/database/sqlite/SQLiteDatabase;

    if-eqz v0, :cond_0

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->isOpen()Z

    move-result v0

    if-eqz v0, :cond_0

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->YjPJqf:Lorg/sqlite/database/sqlite/SQLiteDatabase;

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->close()V

    const/4 v0, 0x0

    iput-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->YjPJqf:Lorg/sqlite/database/sqlite/SQLiteDatabase;
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    :cond_0
    monitor-exit p0

    return-void

    :cond_1
    :try_start_1
    new-instance v0, Ljava/lang/IllegalStateException;

    const-string v1, "XgFZIO\u000eI]mcNE\u0007OKYS[HPRD\\LFU_"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    invoke-direct {v0, v1}, Ljava/lang/IllegalStateException;-><init>(Ljava/lang/String;)V

    throw v0
    :try_end_1
    .catchall {:try_start_1 .. :try_end_1} :catchall_0

    :catchall_0
    move-exception v0

    monitor-exit p0

    throw v0
.end method

.method public getDatabaseName()Ljava/lang/String;
    .locals 1

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->TqOptV:Ljava/lang/String;

    return-object v0
.end method

.method public getReadableDatabase()Lorg/sqlite/database/sqlite/SQLiteDatabase;
    .locals 1

    monitor-enter p0

    const/4 v0, 0x0

    :try_start_0
    invoke-direct {p0, v0}, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->QOuBxx(Z)Lorg/sqlite/database/sqlite/SQLiteDatabase;

    move-result-object v0

    monitor-exit p0

    return-object v0

    :catchall_0
    move-exception v0

    monitor-exit p0
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    throw v0
.end method

.method public getWritableDatabase()Lorg/sqlite/database/sqlite/SQLiteDatabase;
    .locals 1

    monitor-enter p0

    const/4 v0, 0x1

    :try_start_0
    invoke-direct {p0, v0}, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->QOuBxx(Z)Lorg/sqlite/database/sqlite/SQLiteDatabase;

    move-result-object v0

    monitor-exit p0

    return-object v0

    :catchall_0
    move-exception v0

    monitor-exit p0
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    throw v0
.end method

.method public onBeforeDelete(Lorg/sqlite/database/sqlite/SQLiteDatabase;)V
    .locals 0

    return-void
.end method

.method public onConfigure(Lorg/sqlite/database/sqlite/SQLiteDatabase;)V
    .locals 0

    return-void
.end method

.method public abstract onCreate(Lorg/sqlite/database/sqlite/SQLiteDatabase;)V
.end method

.method public onDowngrade(Lorg/sqlite/database/sqlite/SQLiteDatabase;II)V
    .locals 2

    new-instance p1, Lorg/sqlite/database/sqlite/SQLiteException;

    new-instance v0, Ljava/lang/StringBuilder;

    invoke-direct {v0}, Ljava/lang/StringBuilder;-><init>()V

    const-string v1, "XjG\u000eX\u000bJB_qmRCCC\u0005TFFH^ZMX\u0018IH^Y\u0013@PREIWD/"

    invoke-static {v1}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v1

    invoke-virtual {v0, v1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    invoke-virtual {v0, p2}, Ljava/lang/StringBuilder;->append(I)Ljava/lang/StringBuilder;

    move-result-object p2

    const-string v0, ";\u007fF\t"

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object v0

    invoke-virtual {p2, v0}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object p2

    invoke-virtual {p2, p3}, Ljava/lang/StringBuilder;->append(I)Ljava/lang/StringBuilder;

    move-result-object p2

    invoke-virtual {p2}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object p2

    invoke-direct {p1, p2}, Lorg/sqlite/database/sqlite/SQLiteException;-><init>(Ljava/lang/String;)V

    throw p1
.end method

.method public onOpen(Lorg/sqlite/database/sqlite/SQLiteDatabase;)V
    .locals 0

    return-void
.end method

.method public abstract onUpgrade(Lorg/sqlite/database/sqlite/SQLiteDatabase;II)V
.end method

.method public setWriteAheadLoggingEnabled(Z)V
    .locals 1

    monitor-enter p0

    :try_start_0
    iget-boolean v0, p0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->CmrnFC:Z

    if-eq v0, p1, :cond_2

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->YjPJqf:Lorg/sqlite/database/sqlite/SQLiteDatabase;

    if-eqz v0, :cond_1

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->isOpen()Z

    move-result v0

    if-eqz v0, :cond_1

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->YjPJqf:Lorg/sqlite/database/sqlite/SQLiteDatabase;

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->isReadOnly()Z

    move-result v0

    if-nez v0, :cond_1

    if-eqz p1, :cond_0

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->YjPJqf:Lorg/sqlite/database/sqlite/SQLiteDatabase;

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->enableWriteAheadLogging()Z

    goto :goto_0

    :cond_0
    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->YjPJqf:Lorg/sqlite/database/sqlite/SQLiteDatabase;

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->disableWriteAheadLogging()V

    :cond_1
    :goto_0
    iput-boolean p1, p0, Lorg/sqlite/database/sqlite/SQLiteOpenHelper;->CmrnFC:Z

    :cond_2
    monitor-exit p0

    return-void

    :catchall_0
    move-exception p1

    monitor-exit p0
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    throw p1
.end method
