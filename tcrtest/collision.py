from tqdm import tqdm
from concurrent.futures import ProcessPoolExecutor, as_completed
import itertools
from functools import partial

"""
This module does one thing it gets all the unique keys in a set of sequences
given a masking function.

Only get_unique_keys is meant to be used externally.
"""


def collision(x):
    """
    Generates all possible single substitutions and insertions of a period ('.') in the input string.
    
    Parameters:
    x : str
        The input string for which collision variations are to be generated.
    
    Returns:
    list
        A list containing strings with each possible single substitution and insertion of a period.
    """
    subs   = [x[:i-1] + "." + x[i:] for i in range(1, len(x)+1)] 
    indels = [x[:i]   + "." + x[i:] for i in range(1, len(x)+1)]
    return subs + indels

def collision2(x):
    """
    Generates all combinations of the input string where two different characters are replaced by periods.
    
    Parameters:
    x : str
        The input string for which combinations with two periods are to be generated.
    
    Returns:
    list
        A list of strings, each with two characters from the original string replaced by periods.
    """
    n = len(x)
    subs = []
    for i in range(n):
        for j in range(i + 1, n):
            char_list = list(x)
            char_list[i], char_list[j] = '.', '.'
            subs.append(''.join(char_list))
    return subs

def kmer_collision(x, k = 5):
    """
    Generates all possible k-mers (substrings of length k) from the input string.
    
    Parameters:
    x : str
        The input string from which k-mers are to be generated.
    k : int, optional
        The length of each k-mer. Default is 5.
    
    Returns:
    list
        A list of k-mer strings derived from the input string.
    """
    kmers = ["".join(x[i:i+k]) for i in range(0,(len(x)-k))]
    return kmers

def cdr3_5mers_dis13_collision(x):
    """
    Generates a list of 5-mer patterns from the input string with specific character distances.
    The pattern includes every alternate character up to the fifth position from each starting point.
    
    Parameters:
    x : str
        The input string from which 5-mer patterns are to be generated.
    
    Returns:
    list
        A list of strings, each consisting of characters from the input string at positions 
        i, i+2, and i+4 for each starting index i up to the length of the string minus 4.
    """
    cdr3_5mers_dis13 = [x[i]+"."+ x[i+2] +"."+x[i+4] for i in range(0,(len(x)-4))]
    return cdr3_5mers_dis13 

def cdr3_5mers_dis2_collision(x):
    """
    Generates a list of segmented 5-mer patterns from the input string where the middle character is omitted.
    
    Parameters:
    x : str
        The input string from which segmented 5-mer patterns are to be generated.
    
    Returns:
    list
        A list of strings, each consisting of the first two characters, followed by a period, 
        and then the last two characters of each 5-character segment starting from each index.
    """
    cdr3_4mers_dis3= ["".join(x[i:i+2])+"."+ "".join(x[i+3:i+5]) for i in range(0,(len(x)-5))]
    return cdr3_4mers_dis3

def deletion(x):
    """
    Generates all possible single deletions of characters from the input string.
    
    Parameters:
    x : str
        The input string from which deletions are to be generated.
    
    Returns:
    list
        A list containing strings, each with one character removed from the original string.
    """
    dels   = [x[:i-1] + x[i:] for i in range(1, len(x)+1)]
    dels   = dels + [x]
    return dels 

def get_unique_collisions_one_cpu(seqs, collision_func = collision):
    l = list()
    for s in seqs:
        l.extend(collision_func(s))
    return l

def get_unique_collisions(seqs, cpus, collision_func = collision):
    seqs_chunks = [x for x in chunk_list_(seqs, cpus)]
    action = partial(get_keys_from_seqs_, collision_func = collision_func)
    with ProcessPoolExecutor(max_workers=cpus) as executor:
        results = list(  tqdm(executor.map(action, seqs_chunks),total=len(seqs_chunks)))
    results = list(itertools.chain.from_iterable(results))
    return list(results)

def get_keys_from_seqs_(seqs, collision_func):
    l = list()
    for s in seqs:
        l.extend(collision_func(s))
    return(l)

def chunk_list_(lst, n):
    """Splits the list `lst` into `n` roughly equal parts."""
    k, m = divmod(len(lst), n)
    return (lst[i * k + min(i, m):(i + 1) * k + min(i + 1, m)] for i in range(n))
