// source: http://paulbourke.net/miscellaneous/dft/fft_ms.c
// see: http://paulbourke.net/miscellaneous/dft/
namespace fft {
using std::complex;

/*
   Modification of Paul Bourkes FFT code by Peter Cusack 
   to utilise the Microsoft complex type.

   This computes an in-place complex-to-complex FFT 
   x and y are the real and imaginary arrays of 2^m points.
   dir =  1 gives forward transform
   dir = -1 gives reverse transform 
*/
static void FFT(int dir, long n, long m, complex <double> x[])
{
   long i, i1, i2,j, k, l, l1, l2;
   complex <double> tx, t1, u, c;

   /* Do the bit reversal */
   i2 = n >> 1;
   j = 0;

   for (i = 0; i < n-1 ; i++)
   {
      if (i < j)
         swap(x[i], x[j]);

      k = i2;

      while (k <= j) 
	  {
         j -= k;
         k >>= 1;
      }

      j += k;
   }

   /* Compute the FFT */
   c.real(-1.0);
   c.imag(0.0);
   l2 = 1;
   for (l = 0; l < m; l++) 
   {
      l1 = l2;
      l2 <<= 1;
      u.real(1.0);
      u.imag(0.0);

      for (j = 0; j < l1; j++) 
	  {
         for (i = j; i < n; i += l2) 
		 {
            i1 = i + l1;
            t1 = u * x[i1];
            x[i1] = x[i] - t1; 
            x[i] += t1;
         }

         u = u * c;
      }

      c.imag(sqrt((1.0 - c.real()) / 2.0));
      if (dir == 1)
         c.imag(-c.imag());
      c.real(sqrt((1.0 + c.real()) / 2.0));
   }

   /* Scaling for forward transform */
   if (dir == 1) 
   {
      for (i = 0; i < n; i++)
         x[i] /= n;      
   }   
   return;
}

}
