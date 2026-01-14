.class public Lorg/sqlite/database/sqlite/SQLiteCursor;
.super Landroid/database/AbstractWindowedCursor;


# static fields
.field static MBeOHb:Ljava/lang/String; = null
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Lint;"
        }
    .end annotation
.end field

.field static final NO_COUNT:I = -0x1

.field static final TAG:Ljava/lang/String; = "SQLiteCursor"

.field static TMAhKk:Ljava/lang/String;
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Lint;"
        }
    .end annotation
.end field

.field static iOXtbA:Ljava/lang/String;
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Lint;"
        }
    .end annotation
.end field

.field static idVSBv:Ljava/lang/String;
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Lint;"
        }
    .end annotation
.end field

.field static mLQCkb:Ljava/lang/String;
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Lint;"
        }
    .end annotation
.end field


# instance fields
.field private BxLFRT:I

.field private final CiRZnA:Lorg/sqlite/database/sqlite/SQLiteCursorDriver;

.field private final CmNvsS:[Ljava/lang/String;

.field private final EQVAdg:Lorg/sqlite/database/sqlite/SQLiteQuery;

.field private final ThTaYB:Ljava/lang/String;

.field private TthODW:Ljava/util/Map;
    .annotation system Ldalvik/annotation/Signature;
        value = {
            "Ljava/util/Map<",
            "Ljava/lang/String;",
            "Ljava/lang/Integer;",
            ">;"
        }
    .end annotation
.end field

.field private mqOjkD:I


# direct methods
.method static constructor <clinit>()V
    .locals 1

    const/4 v0, 0x0

    invoke-static {v0}, Lorg/sqlite/database/sqlite/SQLiteCursor;->FMD(Z)V

    return-void
.end method

.method public constructor <init>(Lorg/sqlite/database/sqlite/SQLiteCursorDriver;Ljava/lang/String;Lorg/sqlite/database/sqlite/SQLiteQuery;)V
    .locals 1

    invoke-direct {p0}, Landroid/database/AbstractWindowedCursor;-><init>()V

    const/4 v0, -0x1

    iput v0, p0, Lorg/sqlite/database/sqlite/SQLiteCursor;->BxLFRT:I

    if-eqz p3, :cond_0

    iput-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteCursor;->CiRZnA:Lorg/sqlite/database/sqlite/SQLiteCursorDriver;

    iput-object p2, p0, Lorg/sqlite/database/sqlite/SQLiteCursor;->ThTaYB:Ljava/lang/String;

    const/4 p1, 0x0

    iput-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteCursor;->TthODW:Ljava/util/Map;

    iput-object p3, p0, Lorg/sqlite/database/sqlite/SQLiteCursor;->EQVAdg:Lorg/sqlite/database/sqlite/SQLiteQuery;

    invoke-virtual {p3}, Lorg/sqlite/database/sqlite/SQLiteQuery;->getColumnNames()[Ljava/lang/String;

    move-result-object p1

    iput-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteCursor;->CmNvsS:[Ljava/lang/String;

    return-void

    :cond_0
    new-instance p1, Ljava/lang/IllegalArgumentException;

    sget-object p2, Lorg/sqlite/database/sqlite/SQLiteCursor;->mLQCkb:Ljava/lang/String;

    invoke-direct {p1, p2}, Ljava/lang/IllegalArgumentException;-><init>(Ljava/lang/String;)V

    throw p1
.end method

.method public constructor <init>(Lorg/sqlite/database/sqlite/SQLiteDatabase;Lorg/sqlite/database/sqlite/SQLiteCursorDriver;Ljava/lang/String;Lorg/sqlite/database/sqlite/SQLiteQuery;)V
    .locals 0
    .annotation runtime Ljava/lang/Deprecated;
    .end annotation

    invoke-direct {p0, p2, p3, p4}, Lorg/sqlite/database/sqlite/SQLiteCursor;-><init>(Lorg/sqlite/database/sqlite/SQLiteCursorDriver;Ljava/lang/String;Lorg/sqlite/database/sqlite/SQLiteQuery;)V

    return-void
.end method

.method public static synthetic FMD(Z)V
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

    invoke-static {p0}, Lorg/sqlite/database/sqlite/SQLiteCursor;->FMD(Z)V

    :cond_0
    const-string p0, "j~L[U\u000bAOBziT\u0002DGK^HF\t^^\u001eSMCV"

    invoke-static {p0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    sput-object p0, Lorg/sqlite/database/sqlite/SQLiteCursor;->mLQCkb:Ljava/lang/String;

    const-string p0, "HZe@XNmXZleR"

    invoke-static {p0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    sput-object p0, Lorg/sqlite/database/sqlite/SQLiteCursor;->MBeOHb:Ljava/lang/String;

    const-string p0, "inJLE]KI\u0008|eULS\u000e\u000f\u0019\u0007T[SV\u001eSY[SGQlP\\LZ\u007fOCajb\u007f\u0004\u0008"

    invoke-static {p0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    sput-object p0, Lorg/sqlite/database/sqlite/SQLiteCursor;->idVSBv:Ljava/lang/String;

    const-string p0, "inX\\IYW\u0005\u0001?lAKKCA\u0010"

    invoke-static {p0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    sput-object p0, Lorg/sqlite/database/sqlite/SQLiteCursor;->TMAhKk:Ljava/lang/String;

    const-string p0, "inX\\IXZDFx*CMKSH^\u0007\\HQ^\u001eJQ[R\u0011@RTYE\u0016NYGj. %\u001e"

    invoke-static {p0}, Lorg/sqlite/database/sqlite/SQLiteConnectionPool;->qEJ(Ljava/lang/String;)Ljava/lang/String;

    move-result-object p0

    sput-object p0, Lorg/sqlite/database/sqlite/SQLiteCursor;->iOXtbA:Ljava/lang/String;

    return-void
.end method

.method private YzVIBs()V
    .locals 1

    const/4 v0, 0x0

    invoke-virtual {p0, v0}, Lorg/sqlite/database/sqlite/SQLiteCursor;->setWindow(Landroid/database/CursorWindow;)V

    return-void
.end method

.method private mSFJAF(Ljava/lang/String;)V
    .locals 1

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteCursor;->getWindow()Landroid/database/CursorWindow;

    move-result-object v0

    if-nez v0, :cond_0

    new-instance v0, Landroid/database/CursorWindow;

    invoke-direct {v0, p1}, Landroid/database/CursorWindow;-><init>(Ljava/lang/String;)V

    invoke-virtual {p0, v0}, Lorg/sqlite/database/sqlite/SQLiteCursor;->setWindow(Landroid/database/CursorWindow;)V

    goto :goto_0

    :cond_0
    invoke-virtual {v0}, Landroid/database/CursorWindow;->clear()V

    :goto_0
    return-void
.end method

.method private vQTHHc(I)V
    .locals 4

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteCursor;->getDatabase()Lorg/sqlite/database/sqlite/SQLiteDatabase;

    move-result-object v0

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->getPath()Ljava/lang/String;

    move-result-object v0

    invoke-direct {p0, v0}, Lorg/sqlite/database/sqlite/SQLiteCursor;->mSFJAF(Ljava/lang/String;)V

    :try_start_0
    iget v0, p0, Lorg/sqlite/database/sqlite/SQLiteCursor;->BxLFRT:I

    const/4 v1, -0x1

    const/4 v2, 0x0

    if-ne v0, v1, :cond_0

    invoke-static {p1, v2}, Lorg/sqlite/database/DatabaseUtils;->cursorPickFillWindowStartPosition(II)I

    move-result v0

    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteCursor;->EQVAdg:Lorg/sqlite/database/sqlite/SQLiteQuery;

    iget-object v2, p0, Lorg/sqlite/database/sqlite/SQLiteCursor;->mWindow:Landroid/database/CursorWindow;

    const/4 v3, 0x1

    invoke-virtual {v1, v2, v0, p1, v3}, Lorg/sqlite/database/sqlite/SQLiteQuery;->fillWindow(Landroid/database/CursorWindow;IIZ)I

    move-result p1

    iput p1, p0, Lorg/sqlite/database/sqlite/SQLiteCursor;->BxLFRT:I

    iget-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteCursor;->mWindow:Landroid/database/CursorWindow;

    invoke-virtual {p1}, Landroid/database/CursorWindow;->getNumRows()I

    move-result p1

    iput p1, p0, Lorg/sqlite/database/sqlite/SQLiteCursor;->mqOjkD:I

    sget-object p1, Lorg/sqlite/database/sqlite/SQLiteCursor;->MBeOHb:Ljava/lang/String;

    const/4 v0, 0x3

    invoke-static {p1, v0}, Landroid/util/Log;->isLoggable(Ljava/lang/String;I)Z

    move-result p1

    if-eqz p1, :cond_1

    sget-object p1, Lorg/sqlite/database/sqlite/SQLiteCursor;->MBeOHb:Ljava/lang/String;

    new-instance v0, Ljava/lang/StringBuilder;

    invoke-direct {v0}, Ljava/lang/StringBuilder;-><init>()V

    sget-object v1, Lorg/sqlite/database/sqlite/SQLiteCursor;->idVSBv:Ljava/lang/String;

    invoke-virtual {v0, v1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v0

    iget v1, p0, Lorg/sqlite/database/sqlite/SQLiteCursor;->BxLFRT:I

    invoke-virtual {v0, v1}, Ljava/lang/StringBuilder;->append(I)Ljava/lang/StringBuilder;

    move-result-object v0

    invoke-virtual {v0}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v0

    invoke-static {p1, v0}, Landroid/util/Log;->d(Ljava/lang/String;Ljava/lang/String;)I

    goto :goto_0

    :cond_0
    iget v0, p0, Lorg/sqlite/database/sqlite/SQLiteCursor;->mqOjkD:I

    invoke-static {p1, v0}, Lorg/sqlite/database/DatabaseUtils;->cursorPickFillWindowStartPosition(II)I

    move-result v0

    iget-object v1, p0, Lorg/sqlite/database/sqlite/SQLiteCursor;->EQVAdg:Lorg/sqlite/database/sqlite/SQLiteQuery;

    iget-object v3, p0, Lorg/sqlite/database/sqlite/SQLiteCursor;->mWindow:Landroid/database/CursorWindow;

    invoke-virtual {v1, v3, v0, p1, v2}, Lorg/sqlite/database/sqlite/SQLiteQuery;->fillWindow(Landroid/database/CursorWindow;IIZ)I
    :try_end_0
    .catch Ljava/lang/RuntimeException; {:try_start_0 .. :try_end_0} :catch_0

    :cond_1
    :goto_0
    return-void

    :catch_0
    move-exception p1

    invoke-direct {p0}, Lorg/sqlite/database/sqlite/SQLiteCursor;->YzVIBs()V

    throw p1
.end method


# virtual methods
.method public close()V
    .locals 1

    invoke-super {p0}, Landroid/database/AbstractWindowedCursor;->close()V

    monitor-enter p0

    :try_start_0
    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteCursor;->EQVAdg:Lorg/sqlite/database/sqlite/SQLiteQuery;

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteQuery;->close()V

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteCursor;->CiRZnA:Lorg/sqlite/database/sqlite/SQLiteCursorDriver;

    invoke-interface {v0}, Lorg/sqlite/database/sqlite/SQLiteCursorDriver;->cursorClosed()V

    monitor-exit p0

    return-void

    :catchall_0
    move-exception v0

    monitor-exit p0
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    throw v0
.end method

.method public deactivate()V
    .locals 1

    invoke-super {p0}, Landroid/database/AbstractWindowedCursor;->deactivate()V

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteCursor;->CiRZnA:Lorg/sqlite/database/sqlite/SQLiteCursorDriver;

    invoke-interface {v0}, Lorg/sqlite/database/sqlite/SQLiteCursorDriver;->cursorDeactivated()V

    return-void
.end method

.method protected finalize()V
    .locals 1

    :try_start_0
    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteCursor;->mWindow:Landroid/database/CursorWindow;

    if-eqz v0, :cond_0

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteCursor;->close()V
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    :cond_0
    invoke-super {p0}, Landroid/database/AbstractWindowedCursor;->finalize()V

    return-void

    :catchall_0
    move-exception v0

    invoke-super {p0}, Landroid/database/AbstractWindowedCursor;->finalize()V

    throw v0
.end method

.method public getColumnIndex(Ljava/lang/String;)I
    .locals 6

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteCursor;->TthODW:Ljava/util/Map;

    if-nez v0, :cond_1

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteCursor;->CmNvsS:[Ljava/lang/String;

    array-length v1, v0

    new-instance v2, Ljava/util/HashMap;

    const/high16 v3, 0x3f800000    # 1.0f

    invoke-direct {v2, v1, v3}, Ljava/util/HashMap;-><init>(IF)V

    const/4 v3, 0x0

    :goto_0
    if-ge v3, v1, :cond_0

    aget-object v4, v0, v3

    invoke-static {v3}, Ljava/lang/Integer;->valueOf(I)Ljava/lang/Integer;

    move-result-object v5

    invoke-virtual {v2, v4, v5}, Ljava/util/HashMap;->put(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;

    add-int/lit8 v3, v3, 0x1

    goto :goto_0

    :cond_0
    iput-object v2, p0, Lorg/sqlite/database/sqlite/SQLiteCursor;->TthODW:Ljava/util/Map;

    :cond_1
    const/16 v0, 0x2e

    invoke-virtual {p1, v0}, Ljava/lang/String;->lastIndexOf(I)I

    move-result v0

    const/4 v1, -0x1

    if-eq v0, v1, :cond_2

    new-instance v2, Ljava/lang/Exception;

    invoke-direct {v2}, Ljava/lang/Exception;-><init>()V

    sget-object v3, Lorg/sqlite/database/sqlite/SQLiteCursor;->MBeOHb:Ljava/lang/String;

    new-instance v4, Ljava/lang/StringBuilder;

    invoke-direct {v4}, Ljava/lang/StringBuilder;-><init>()V

    sget-object v5, Lorg/sqlite/database/sqlite/SQLiteCursor;->iOXtbA:Ljava/lang/String;

    invoke-virtual {v4, v5}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v4

    invoke-virtual {v4, p1}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v4

    invoke-virtual {v4}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v4

    invoke-static {v3, v4, v2}, Landroid/util/Log;->e(Ljava/lang/String;Ljava/lang/String;Ljava/lang/Throwable;)I

    add-int/lit8 v0, v0, 0x1

    invoke-virtual {p1, v0}, Ljava/lang/String;->substring(I)Ljava/lang/String;

    move-result-object p1

    :cond_2
    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteCursor;->TthODW:Ljava/util/Map;

    invoke-interface {v0, p1}, Ljava/util/Map;->get(Ljava/lang/Object;)Ljava/lang/Object;

    move-result-object p1

    check-cast p1, Ljava/lang/Integer;

    if-eqz p1, :cond_3

    invoke-virtual {p1}, Ljava/lang/Integer;->intValue()I

    move-result p1

    return p1

    :cond_3
    return v1
.end method

.method public getColumnNames()[Ljava/lang/String;
    .locals 1

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteCursor;->CmNvsS:[Ljava/lang/String;

    return-object v0
.end method

.method public getCount()I
    .locals 2

    iget v0, p0, Lorg/sqlite/database/sqlite/SQLiteCursor;->BxLFRT:I

    const/4 v1, -0x1

    if-ne v0, v1, :cond_0

    const/4 v0, 0x0

    invoke-direct {p0, v0}, Lorg/sqlite/database/sqlite/SQLiteCursor;->vQTHHc(I)V

    :cond_0
    iget v0, p0, Lorg/sqlite/database/sqlite/SQLiteCursor;->BxLFRT:I

    return v0
.end method

.method public getDatabase()Lorg/sqlite/database/sqlite/SQLiteDatabase;
    .locals 1

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteCursor;->EQVAdg:Lorg/sqlite/database/sqlite/SQLiteQuery;

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteQuery;->getDatabase()Lorg/sqlite/database/sqlite/SQLiteDatabase;

    move-result-object v0

    return-object v0
.end method

.method public onMove(II)Z
    .locals 1

    iget-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteCursor;->mWindow:Landroid/database/CursorWindow;

    if-eqz p1, :cond_0

    iget-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteCursor;->mWindow:Landroid/database/CursorWindow;

    invoke-virtual {p1}, Landroid/database/CursorWindow;->getStartPosition()I

    move-result p1

    if-lt p2, p1, :cond_0

    iget-object p1, p0, Lorg/sqlite/database/sqlite/SQLiteCursor;->mWindow:Landroid/database/CursorWindow;

    invoke-virtual {p1}, Landroid/database/CursorWindow;->getStartPosition()I

    move-result p1

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteCursor;->mWindow:Landroid/database/CursorWindow;

    invoke-virtual {v0}, Landroid/database/CursorWindow;->getNumRows()I

    move-result v0

    add-int/2addr p1, v0

    if-lt p2, p1, :cond_1

    :cond_0
    invoke-direct {p0, p2}, Lorg/sqlite/database/sqlite/SQLiteCursor;->vQTHHc(I)V

    :cond_1
    const/4 p1, 0x1

    return p1
.end method

.method public requery()Z
    .locals 5

    invoke-virtual {p0}, Lorg/sqlite/database/sqlite/SQLiteCursor;->isClosed()Z

    move-result v0

    const/4 v1, 0x0

    if-eqz v0, :cond_0

    return v1

    :cond_0
    monitor-enter p0

    :try_start_0
    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteCursor;->EQVAdg:Lorg/sqlite/database/sqlite/SQLiteQuery;

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteQuery;->getDatabase()Lorg/sqlite/database/sqlite/SQLiteDatabase;

    move-result-object v0

    invoke-virtual {v0}, Lorg/sqlite/database/sqlite/SQLiteDatabase;->isOpen()Z

    move-result v0

    if-nez v0, :cond_1

    monitor-exit p0

    return v1

    :cond_1
    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteCursor;->mWindow:Landroid/database/CursorWindow;

    if-eqz v0, :cond_2

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteCursor;->mWindow:Landroid/database/CursorWindow;

    invoke-virtual {v0}, Landroid/database/CursorWindow;->clear()V

    :cond_2
    const/4 v0, -0x1

    iput v0, p0, Lorg/sqlite/database/sqlite/SQLiteCursor;->mPos:I

    iput v0, p0, Lorg/sqlite/database/sqlite/SQLiteCursor;->BxLFRT:I

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteCursor;->CiRZnA:Lorg/sqlite/database/sqlite/SQLiteCursorDriver;

    invoke-interface {v0, p0}, Lorg/sqlite/database/sqlite/SQLiteCursorDriver;->cursorRequeried(Landroid/database/Cursor;)V

    monitor-exit p0
    :try_end_0
    .catchall {:try_start_0 .. :try_end_0} :catchall_0

    :try_start_1
    invoke-super {p0}, Landroid/database/AbstractWindowedCursor;->requery()Z

    move-result v0
    :try_end_1
    .catch Ljava/lang/IllegalStateException; {:try_start_1 .. :try_end_1} :catch_0

    return v0

    :catch_0
    move-exception v0

    sget-object v2, Lorg/sqlite/database/sqlite/SQLiteCursor;->MBeOHb:Ljava/lang/String;

    new-instance v3, Ljava/lang/StringBuilder;

    invoke-direct {v3}, Ljava/lang/StringBuilder;-><init>()V

    sget-object v4, Lorg/sqlite/database/sqlite/SQLiteCursor;->TMAhKk:Ljava/lang/String;

    invoke-virtual {v3, v4}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v3

    invoke-virtual {v0}, Ljava/lang/IllegalStateException;->getMessage()Ljava/lang/String;

    move-result-object v4

    invoke-virtual {v3, v4}, Ljava/lang/StringBuilder;->append(Ljava/lang/String;)Ljava/lang/StringBuilder;

    move-result-object v3

    invoke-virtual {v3}, Ljava/lang/StringBuilder;->toString()Ljava/lang/String;

    move-result-object v3

    invoke-static {v2, v3, v0}, Landroid/util/Log;->w(Ljava/lang/String;Ljava/lang/String;Ljava/lang/Throwable;)I

    return v1

    :catchall_0
    move-exception v0

    :try_start_2
    monitor-exit p0
    :try_end_2
    .catchall {:try_start_2 .. :try_end_2} :catchall_0

    throw v0
.end method

.method public setSelectionArguments([Ljava/lang/String;)V
    .locals 1

    iget-object v0, p0, Lorg/sqlite/database/sqlite/SQLiteCursor;->CiRZnA:Lorg/sqlite/database/sqlite/SQLiteCursorDriver;

    invoke-interface {v0, p1}, Lorg/sqlite/database/sqlite/SQLiteCursorDriver;->setBindArguments([Ljava/lang/String;)V

    return-void
.end method

.method public setWindow(Landroid/database/CursorWindow;)V
    .locals 0

    invoke-super {p0, p1}, Landroid/database/AbstractWindowedCursor;->setWindow(Landroid/database/CursorWindow;)V

    const/4 p1, -0x1

    iput p1, p0, Lorg/sqlite/database/sqlite/SQLiteCursor;->BxLFRT:I

    return-void
.end method
